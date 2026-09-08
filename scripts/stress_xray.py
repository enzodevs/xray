#!/usr/bin/env python3
"""Bounded, repeatable read-only stress sampling of real repositories/snapshots.

Writes metrics and command output outside tested projects. No project code executes.
Exit 1 means a command failed, timed out, or returned invalid digest JSON, not that
all successful digests are semantically correct; source review is still required.
"""
import argparse
import collections
import hashlib
import json
import math
import os
import platform
from pathlib import Path
import signal
import subprocess
import time

EXTENSIONS = {'.ts', '.tsx', '.js', '.jsx', '.go', '.py', '.php', '.rs', '.sql', '.md', '.vue', '.svelte'}
SKIP = {'node_modules', 'vendor', 'target', 'dist', 'build', 'coverage', '__pycache__'}


def run(command, cwd, timeout):
    started = time.monotonic()
    process = subprocess.Popen(command, cwd=cwd, stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE, start_new_session=True)
    expired = False
    try:
        stdout, stderr = process.communicate(timeout=timeout)
    except subprocess.TimeoutExpired:
        expired = True
        os.killpg(process.pid, signal.SIGKILL)
        stdout, stderr = process.communicate()
    return process.returncode, expired, time.monotonic() - started, stdout, stderr


def candidates(root):
    listing = subprocess.run(['git', 'ls-files', '-z'], cwd=root, capture_output=True)
    if listing.returncode == 0:
        relatives = [Path(os.fsdecode(raw)) for raw in listing.stdout.split(b'\0') if raw]
    else:
        # opensrc downloads are frequently snapshots without .git metadata.
        if (root / '.git').exists():
            raise RuntimeError(f'Git enumeration failed for {root}: {listing.stderr!r}')
        relatives = []
        for directory, dirs, files in os.walk(root, followlinks=False):
            dirs[:] = sorted(d for d in dirs if d not in SKIP and not d.startswith('.')
                             and not (Path(directory) / d).is_symlink())
            relatives.extend((Path(directory) / name).relative_to(root) for name in sorted(files))
    groups = collections.defaultdict(list)
    for relative in relatives:
        path = root / relative
        if (relative.suffix not in EXTENSIONS
                or any(part in SKIP or part.startswith('.') for part in relative.parts[:-1])
                or any(p.is_symlink() for p in [path, *path.parents] if p != root)
                or not path.is_file()):
            continue
        groups[relative.suffix].append(relative)
    return groups


def sample(files, root, count):
    # Stable hash avoids alphabetic bias; always include the three largest inputs.
    largest = sorted(files, key=lambda p: (-(root / p).stat().st_size, str(p)))[:3]
    hashed = sorted(files, key=lambda p: hashlib.sha256(str(p).encode()).digest())
    return list(dict.fromkeys(largest + hashed))[:count]


def validate_digest(data):
    assert data['schema_version'] == 1, 'unexpected schema version'
    assert data['mode'] == 'digest', 'unexpected mode'
    file = data['file']
    assert file['total_lines'] >= 0, 'negative line count'
    content = file['content']['data']
    for group in ('exports', 'internals'):
        for symbol in content.get(group, []):
            assert symbol['name'], 'empty symbol name'
            span = symbol['span']
            assert 1 <= span['start_line'] <= span['end_line'] <= file['total_lines'], ('invalid span', span)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--binary', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--per-language', type=int, default=40)
    parser.add_argument('--timeout', type=float, default=20)
    parser.add_argument('roots', nargs='+', type=Path)
    args = parser.parse_args()
    if args.per_language < 1 or args.timeout <= 0:
        parser.error('--per-language and --timeout must be positive')
    if args.output.exists() and any(args.output.iterdir()):
        parser.error('--output must be a new or empty directory')
    binary = str(args.binary.resolve())
    args.output.mkdir(parents=True, exist_ok=True)
    metadata = {'binary': binary, 'binary_sha256': hashlib.sha256(Path(binary).read_bytes()).hexdigest(),
                'platform': platform.platform(), 'python': platform.python_version(),
                'per_language': args.per_language, 'timeout_seconds': args.timeout,
                'roots': [str(root.resolve()) for root in args.roots]}
    (args.output / 'metadata.json').write_text(json.dumps(metadata, indent=2) + '\n')
    results = []
    inventory = []
    for root in args.roots:
        root = root.resolve()
        groups = candidates(root)
        inventory.append({'root': str(root), 'counts': {k: len(v) for k, v in groups.items()}})
        for extension, files in sorted(groups.items()):
            selected = sample(files, root, args.per_language)
            for index, relative in enumerate(selected):
                modes = [('json', ['--format', 'json']), ('text', ['--no-follow'])]
                if index < 5:
                    modes.append(('follow', ['--explain', '--depth', '2']))
                for mode, flags in modes:
                    command = [binary, *flags, str(relative)]
                    code, expired, elapsed, stdout, stderr = run(command, root, args.timeout)
                    identifier = f'{len(results):05d}'
                    (args.output / f'{identifier}.stdout').write_bytes(stdout)
                    (args.output / f'{identifier}.stderr').write_bytes(stderr)
                    error = None
                    if code == 0 and mode == 'json':
                        try:
                            validate_digest(json.loads(stdout))
                        except (ValueError, KeyError, TypeError, AssertionError) as exc:
                            error = str(exc)
                    results.append({'id': identifier, 'root': str(root), 'file': str(relative),
                                    'extension': extension, 'mode': mode, 'command': command,
                                    'exit': code, 'timeout': expired, 'seconds': elapsed,
                                    'source_bytes': (root / relative).stat().st_size,
                                    'output_bytes': len(stdout), 'validation_error': error})
                    with (args.output / 'results.jsonl').open('a') as stream:
                        stream.write(json.dumps(results[-1]) + '\n')
            print(f'{root.name} {extension}: {len(selected)}/{len(files)} files', flush=True)
    (args.output / 'results.json').write_text(json.dumps(results, indent=2) + '\n')
    (args.output / 'inventory.json').write_text(json.dumps(inventory, indent=2) + '\n')
    failures = [r for r in results if r['exit'] or r['timeout'] or r['validation_error']]
    summary = {'commands': len(results), 'failures': len(failures), 'by_mode': {}}
    for mode in ('json', 'text', 'follow'):
        rows = [r for r in results if r['mode'] == mode]
        times = sorted(r['seconds'] for r in rows)
        if not times:
            continue
        summary['by_mode'][mode] = {'commands': len(rows), 'p50_seconds': times[len(times)//2],
                                   'p95_seconds': times[math.ceil(len(times)*.95)-1],
                                   'max_seconds': max(times),
                                   'source_bytes': sum(r['source_bytes'] for r in rows),
                                   'output_bytes': sum(r['output_bytes'] for r in rows)}
    (args.output / 'summary.json').write_text(json.dumps(summary, indent=2) + '\n')
    print(json.dumps(summary, indent=2))
    return bool(failures)


if __name__ == '__main__':
    raise SystemExit(main())

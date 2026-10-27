use super::tests::parse_php;
use super::*;

fn symbols(src: &str) -> FileSymbols {
    let tree = parse_php(src.as_bytes());
    assert!(
        !tree.root_node().has_error(),
        "{}",
        tree.root_node().to_sexp()
    );
    let symbols = extract_symbols(tree.root_node(), src.as_bytes());
    assert_eq!(
        symbols.imports,
        extract_sources_only(tree.root_node(), src.as_bytes())
    );
    symbols
}

#[test]
fn dependencies_resolve_current_namespace_fully_qualified_and_relative_names() {
    let result = symbols(
        r"<?php namespace App\Models;
        class Conversation {
            function messages(): Collection { return $this->hasMany(Message::class); }
            function make(?Message $message): Message|\External\Reply {
                new namespace\Draft(); new \External\Job();
                Message::query(); self::boot(); static::boot(); parent::boot();
                return new Conversation();
            }
        }",
    );
    assert_eq!(
        result.imports,
        [
            "App\\Models\\Collection",
            "App\\Models\\Message",
            "External\\Reply",
            "App\\Models\\Draft",
            "External\\Job"
        ]
    );
    assert!(result.import_bindings.is_empty());
}

#[test]
fn dependencies_keep_aliases_and_group_uses_namespace_scoped() {
    let result = symbols(
        r"<?php
        namespace First {
            use Vendor\Package\{Item as Model, Nested};
            use \Vendor\Other as Root;
            new Model(); new Nested\Child(); new Root(); new Local();
        }
        namespace Second {
            use Other\Item as Model;
            new Model(); new Local(); new \Vendor\Package\Item();
        }
        namespace { new GlobalClass(); }",
    );
    assert_eq!(
        result.imports,
        [
            "Vendor\\Package\\Item",
            "Vendor\\Package\\Nested",
            "Vendor\\Other",
            "Vendor\\Package\\Nested\\Child",
            "First\\Local",
            "Other\\Item",
            "Second\\Local",
            "GlobalClass"
        ]
    );
    assert!(
        !result
            .imports
            .iter()
            .any(|name| name.contains("First\\Vendor"))
    );
}

#[test]
fn dependencies_reset_aliases_between_semicolon_namespaces() {
    let result = symbols(
        r"<?php namespace One; use Vendor\Item as Thing; new Thing();
        namespace Two; new Thing(); new namespace\Local();",
    );
    assert_eq!(result.imports, ["Vendor\\Item", "Two\\Thing", "Two\\Local"]);
}

#[test]
fn dependencies_recognize_class_contexts_not_function_property_or_string_names() {
    let result = symbols(
        r#"<?php namespace App;
        use function Vendor\helper as Shared;
        use Vendor\{function other as Utility, const VALUE, Item};
        #[Marker] class Owner extends Base implements Contract {
            use Reusable;
            public Value $property;
            function method(Input $input): Output {
                helper(); Shared(); $input->Ghost(); $input->FalseClass;
                $text = 'Phantom::class'; $text = "new Fake()";
                $dynamic = new $class(); $class::unknown();
                return new Shared();
            }
        }
        new Utility(); new Item();
    "#,
    );
    for name in [
        "App\\Marker",
        "App\\Base",
        "App\\Contract",
        "App\\Reusable",
        "App\\Value",
        "App\\Input",
        "App\\Output",
        "App\\Shared",
        "App\\Utility",
    ] {
        assert!(
            result.imports.iter().any(|item| item == name),
            "missing {name}: {:?}",
            result.imports
        );
    }
    for name in [
        "Ghost",
        "FalseClass",
        "Phantom",
        "Fake",
        "App\\Owner",
        "App\\helper",
    ] {
        assert!(
            !result.imports.iter().any(|item| item.ends_with(name)),
            "false dependency {name}"
        );
    }
}

#[test]
fn dependencies_do_not_infer_include_paths_from_interpolation_or_concatenation() {
    let result = symbols(
        r#"<?php
        require "$root/bootstrap.php";
        include __DIR__ . '/helpers.php';
        require 'plain.php';
        function load() { require './inside.php'; }
    "#,
    );
    assert_eq!(result.imports, ["plain.php", "./inside.php"]);
}

#[test]
fn pest_preserves_labels_nested_groups_and_dataset_span_without_leaking_closures() {
    let result = symbols(
        "<?php\ndescribe('outer', function () {\n    it('first', function () { test('not a declaration'); });\n    describe('inner', function () {\n        test('matrix', fn ($x) => expect($x))->with([\n            'one' => [1],\n        ]);\n    });\n    $deferred = function () { it('not registered'); };\n});\nfunction helper() { it('not top-level'); }\nit('last', fn () => true);\n",
    );
    assert_eq!(result.tests.len(), 2);
    let outer = &result.tests[0];
    assert_eq!(
        (&*outer.kind, &*outer.name, outer.line_start, outer.line_end),
        ("describe", "outer", 2, 10)
    );
    assert_eq!(outer.children.len(), 2);
    assert!(outer.children[0].children.is_empty());
    let matrix = &outer.children[1].children[0];
    assert_eq!(
        (
            &*matrix.kind,
            &*matrix.name,
            matrix.line_start,
            matrix.line_end
        ),
        ("test", "matrix", 5, 7)
    );
    assert_eq!(result.tests[1].name, "last");
    assert_eq!(result.exports.len(), 1);
}

#[test]
fn enclosing_calls_exclude_deferred_closure_and_nested_function_bodies() {
    let result = symbols(
        "<?php function run() { immediate(); defer(function () { deferred(); }); $fn = fn () => hidden(); function nested() { nestedCall(); } after(); }",
    );
    assert_eq!(result.exports[0].calls, ["immediate", "defer", "after"]);
}

#[test]
fn routes_capture_controller_action_name_inherited_prefix_and_middleware() {
    let result = symbols(
        r"<?php
        use Illuminate\Support\Facades\Route as Router;
        use App\Http\Controllers\ProfileController as Profile;
        Router::prefix('api')->name('api.')->middleware(['auth'])->group(function () {
            Router::prefix('v1')->middleware('verified')->group(function () {
                Router::get('profile', [Profile::class, 'show'])
                    ->middleware('throttle:60,1')->name('profile');
            });
        });",
    );
    assert_eq!(result.types.len(), 1);
    let route = &result.types[0];
    assert_eq!(
        (&*route.kind, &*route.name, route.exported),
        ("route", "GET|HEAD /api/v1/profile", false)
    );
    assert_eq!(
        route.summary,
        "controller=App\\Http\\Controllers\\ProfileController action=show name=api.profile middleware=[auth, verified, throttle:60,1]"
    );
    assert_eq!((route.line_start, route.line_end), (6, 7));
    assert!(result.exports.is_empty());
}

#[test]
fn routes_distinguish_deferred_closures_redirects_and_inertia_without_fabricated_calls() {
    let result = symbols(
        r"<?php
        Route::get('/inline', function () { Route::get('/not-registered', $handler); deferred(); });
        Route::redirect('old', '/new');
        Route::inertia('appearance', 'settings/Appearance')->name('appearance');
        function helper() { Route::get('/not-top-level', $handler); }
    ",
    );
    assert_eq!(result.types.len(), 3);
    assert!(result.types[0].summary.starts_with("action=closure"));
    assert_eq!(result.types[1].name, "ANY /old");
    assert!(
        result.types[1]
            .summary
            .starts_with("action=redirect target=/new")
    );
    assert!(
        result.types[2]
            .summary
            .starts_with("action=inertia target=settings/Appearance")
    );
    assert_eq!(result.exports.len(), 1);
}

#[test]
fn routes_support_literal_group_options_and_group_controller() {
    let result = symbols(
        r"<?php
        Route::group(['prefix' => 'admin', 'middleware' => ['auth'], 'as' => 'admin.'], function () {
            Route::controller(\App\Controller::class)->group(function () {
                Route::post('save', 'store')->name('save');
            });
        });",
    );
    assert_eq!(result.types[0].name, "POST /admin/save");
    assert_eq!(
        result.types[0].summary,
        "controller=App\\Controller action=store name=admin.save middleware=[auth]"
    );
}

#[test]
fn routes_mark_dynamic_values_without_guessing_and_ignore_unrelated_facades() {
    let result = symbols(
        r#"<?php
        use Other\Route;
        Route::get('/unrelated', $action);
        \Illuminate\Support\Facades\Route::prefix($prefix)->middleware($middleware)->group(function () {
            \Illuminate\Support\Facades\Route::get("$path", [$controller, $method])->name($name);
        });
    "#,
    );
    assert_eq!(result.types.len(), 1);
    assert_eq!(result.types[0].name, "GET|HEAD <dynamic>");
    assert_eq!(
        result.types[0].summary,
        "controller=<dynamic> action=<dynamic> name=<dynamic> middleware=[<dynamic>]"
    );
}

#[test]
fn class_aliases_do_not_collide_with_function_aliases_in_mixed_groups() {
    let result = symbols(
        r"<?php namespace Demo;
        use Vendor\{Thing as Shared, function helper as Shared};
        new Shared(); Shared();
    ",
    );
    assert_eq!(result.imports, ["Vendor\\Thing", "Vendor\\helper"]);
}

#[test]
fn relative_namespace_keyword_is_case_insensitive() {
    let result = symbols(r"<?php namespace App; new NAMESPACE\Model(); new NaMeSpAcE\Sibling();");
    assert_eq!(result.imports, ["App\\Model", "App\\Sibling"]);
}

#[test]
fn anonymous_class_arguments_are_eager_but_method_bodies_are_not() {
    let result = symbols(
        "<?php function run() { return new class(loadConfig()) { function __construct($config) { deferred(); } function method() { hidden(); } }; }",
    );
    assert_eq!(result.exports[0].calls, ["loadConfig"]);
}

#[test]
fn arrow_route_groups_and_pest_groups_keep_their_expression_bodies() {
    let result = symbols(
        r"<?php
        Route::prefix('api')->name('admin.')->group(fn () => Route::get('profile', fn () => true));
        describe('outer', fn () => it('inner', fn () => true));
    ",
    );
    assert_eq!(result.types.len(), 1);
    assert_eq!(result.types[0].name, "GET|HEAD /api/profile");
    assert_eq!(
        result.types[0].summary,
        "action=closure name=admin. middleware=[]"
    );
    assert_eq!(result.tests[0].children[0].name, "inner");
}

#[test]
fn legacy_group_namespaces_keep_literal_controller_but_mark_identity_unresolved() {
    let result = symbols(
        r"<?php
        Route::namespace('App\Http\Controllers')->group(function () {
            Route::get('profile', 'ProfileController@show');
        });
    ",
    );
    assert_eq!(
        result.types[0].summary,
        "controller=<unresolved:group-namespace> controller_literal=ProfileController action=show name=<none> middleware=[]"
    );
}

#[test]
fn dynamic_group_options_and_named_arguments_are_not_invented() {
    let result = symbols(
        r"<?php
        Route::group($options, function () { Route::get('profile', $action); });
        Route::get(uri: 'named', action: $action);
    ",
    );
    assert_eq!(result.types[0].name, "GET|HEAD <dynamic>");
    assert_eq!(
        result.types[0].summary,
        "action=<dynamic> name=<dynamic> middleware=[<dynamic>]"
    );
    assert_eq!(result.types[1].name, "GET|HEAD <dynamic>");
    assert!(result.types[1].summary.starts_with("action=<dynamic>"));
}

<script lang="ts">
import { service } from './service';
export function submit() {
    return service.save();
}
export function refresh() {
    return 'ready';
}
</script>

<template><span>{{ submit() }}</span></template>

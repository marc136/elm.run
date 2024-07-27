import type { ElmApp, Flags, ToElm } from './UlmEditor.elm'

// helper type to discriminate a union type or get a specific variant from a discriminated union. Eg:
// type DiscriminatedUnion = {tag:'a'; data:string;} | {tag:'b'; data:number;};
// type SpecificVariant = DiscriminateUnion<DiscriminatedUnion, 'tag', 'a'>
// SpecificVariant will be {tag:'a'; data:string;}
export type DiscriminateUnion<T, K extends keyof T, V extends T[K]> = Extract<T, Record<K, V>>

// helper to convert/map a discriminated union type into a Record with discriminator as key
// and variant type as value. Eg:
// type DiscriminatedUnion = {tag:'a'; data:string;} | {tag:'b'; data:number;};
// type Mapped = MapDiscriminatedUnion<DiscriminatedUnion, 'tag'>
// Mapped will be
// { a: { tag: 'a'; data: string; };
//   b: { tag: 'b'; data: number; };
// }
type MapDiscriminatedUnion<T extends Record<K, string>, K extends keyof T> = {
    [V in T[K]]: DiscriminateUnion<T, K, V>
}

type ToElmMap = MapDiscriminatedUnion<ToElm, 'tag'>

export type ToElmRecord<T extends keyof ToElmMap> = ToElmMap[T]
type horst = DiscriminateUnion<ToElm, 'tag', 'compile-result'>
type zwei = ToElmRecord<'compile-result'>['detail']

export type AsCustomEvent<T extends keyof ToElmMap> = ToElmRecord<T>['detail']
type moep = AsCustomEvent<'compile-result'>

export interface ElmInit {
    function init(options: { node?: HTMLElement | null; flags: Flags }): ElmApp;
}

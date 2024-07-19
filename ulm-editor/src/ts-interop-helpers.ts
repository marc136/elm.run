import type { ToElm } from './UlmEditor.elm'

export type DiscriminateUnion<T, K extends keyof T, V extends T[K]> = Extract<T, Record<K, V>>

type MapDiscriminatedUnion<T extends Record<K, string>, K extends keyof T> = {
    [V in T[K]]: DiscriminateUnion<T, K, V>
}

type ToElmMap = MapDiscriminatedUnion<ToElm, 'tag'>

export type ToElmRecord<T extends keyof ToElmMap> = ToElmMap[T]

export type AsCustomEvent<T extends keyof ToElmMap> = ToElmRecord<T>['detail']

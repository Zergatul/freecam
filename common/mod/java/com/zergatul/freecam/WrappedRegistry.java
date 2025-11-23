package com.zergatul.freecam;

import net.minecraft.resources.Identifier;

public interface WrappedRegistry<T> {
    Identifier getKey(T value);
    T getValue(Identifier id);
}
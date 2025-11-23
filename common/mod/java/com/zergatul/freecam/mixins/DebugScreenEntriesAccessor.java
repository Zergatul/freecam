package com.zergatul.freecam.mixins;

import net.minecraft.client.gui.components.debug.DebugScreenEntries;
import net.minecraft.client.gui.components.debug.DebugScreenEntry;
import net.minecraft.resources.Identifier;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Invoker;

@Mixin(DebugScreenEntries.class)
public interface DebugScreenEntriesAccessor {

    @Invoker("register")
    static Identifier register_ZFC(Identifier id, DebugScreenEntry entry) {
        throw new AssertionError();
    }
}
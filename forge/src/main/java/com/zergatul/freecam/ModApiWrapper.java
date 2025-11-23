package com.zergatul.freecam;

import net.minecraft.resources.Identifier;
import net.minecraft.world.level.block.Block;
import net.minecraftforge.event.TickEvent;
import net.minecraftforge.registries.ForgeRegistries;
import net.minecraftforge.registries.IForgeRegistry;

public class ModApiWrapper {

    public static final ModApiWrapper instance = new ModApiWrapper();

    public final WrappedRegistry<Block> BLOCKS = new ForgeWrappedRegistry<>(ForgeRegistries.BLOCKS);

    private ModApiWrapper() {}

    public void init() {
        TickEvent.ClientTickEvent.Pre.BUS.addListener(event -> FreeCam.instance.onClientTickStart());
        TickEvent.ClientTickEvent.Post.BUS.addListener(event -> ChatCommandManager.instance.onClientTickEnd());
    }

    private record ForgeWrappedRegistry<T>(IForgeRegistry<T> registry) implements WrappedRegistry<T> {

        @Override
        public Identifier getKey(T value) {
            return registry.getKey(value);
        }

        @Override
        public T getValue(Identifier id) {
            return registry.getValue(id);
        }
    }
}
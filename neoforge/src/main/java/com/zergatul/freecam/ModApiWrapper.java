package com.zergatul.freecam;

import net.minecraft.core.DefaultedRegistry;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.block.Block;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.neoforge.client.event.ClientTickEvent;

public class ModApiWrapper {

    public static final ModApiWrapper instance = new ModApiWrapper();

    public final WrappedRegistry<Block> BLOCKS = new BuiltInWrappedRegistry<>(BuiltInRegistries.BLOCK);

    private ModApiWrapper() {

    }

    @SuppressWarnings("unused")
    @SubscribeEvent
    public void onPreClientTick(ClientTickEvent.Pre event) {
        FreeCam.instance.onClientTickStart();
    }

    @SuppressWarnings("unused")
    @SubscribeEvent
    public void onPostClientTick(ClientTickEvent.Post event) {
        ChatCommandManager.instance.onClientTickEnd();
    }

    private record BuiltInWrappedRegistry<T>(DefaultedRegistry<T> registry) implements WrappedRegistry<T> {

        @Override
        public ResourceLocation getKey(T value) {
            return registry.getKey(value);
        }

        @Override
        public T getValue(ResourceLocation id) {
            return registry.get(id);
        }
    }
}
package com.zergatul.freecam.fabric;

import com.zergatul.freecam.*;
import net.fabricmc.api.ClientModInitializer;
import net.fabricmc.fabric.api.client.event.lifecycle.v1.ClientTickEvents;
import net.fabricmc.fabric.api.client.keymapping.v1.KeyMappingHelper;
import net.minecraft.core.Registry;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.Identifier;
import net.minecraft.world.level.block.Block;

public class ModMain implements ClientModInitializer {

    @Override
    public void onInitializeClient() {
        KeyMappingHelper.registerKeyMapping(KeyBindings.toggleFreeCam);
        KeyMappingHelper.registerKeyMapping(KeyBindings.toggleCameraLock);
        KeyMappingHelper.registerKeyMapping(KeyBindings.toggleEyeLock);
        KeyMappingHelper.registerKeyMapping(KeyBindings.toggleFollowCam);
        KeyMappingHelper.registerKeyMapping(KeyBindings.startPath);

        DebugScreenExtensions.register();

        ClientTickEvents.START_CLIENT_TICK.register(_ -> FreeCam.instance.onClientTickStart());
        ClientTickEvents.END_CLIENT_TICK.register(_ -> ChatCommandManager.instance.onClientTickEnd());

        ModLoaderBridgeInstance.init(new Bridge());
    }

    private static class Bridge implements ModLoaderBridge {

        @Override
        public WrappedRegistry<Block> getBlockRegistry() {
            return new VanillaWrapperRegistry<>(BuiltInRegistries.BLOCK);
        }
    }

    private record VanillaWrapperRegistry<T>(Registry<T> registry) implements WrappedRegistry<T> {

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
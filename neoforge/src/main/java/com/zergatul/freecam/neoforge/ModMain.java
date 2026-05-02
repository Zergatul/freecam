package com.zergatul.freecam.neoforge;

import com.zergatul.freecam.*;
import com.zergatul.freecam.ui.FreeCamSettingsScreen;
import net.minecraft.core.DefaultedRegistry;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.Identifier;
import net.minecraft.world.level.block.Block;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.neoforge.client.event.ClientTickEvent;
import net.neoforged.neoforge.client.event.RegisterKeyMappingsEvent;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.fml.ModContainer;
import net.neoforged.fml.common.Mod;
import net.neoforged.neoforge.client.gui.IConfigScreenFactory;
import net.neoforged.neoforge.common.NeoForge;

@SuppressWarnings("unused")
@Mod("zergatulfreecam")
public class ModMain {

    public ModMain(IEventBus bus, ModContainer container) {
        bus.addListener(this::onRegisterKeybindings);
        NeoForge.EVENT_BUS.register(new Events());
        DebugScreenExtensions.register();
        container.registerExtensionPoint(
                IConfigScreenFactory.class,
                (cont, screen) -> new FreeCamSettingsScreen(screen));

        ModLoaderBridgeInstance.init(new Bridge());
    }

    private void onRegisterKeybindings(final RegisterKeyMappingsEvent event) {
        event.register(KeyBindings.toggleFreeCam);
        event.register(KeyBindings.toggleCameraLock);
        event.register(KeyBindings.toggleEyeLock);
        event.register(KeyBindings.toggleFollowCam);
        event.register(KeyBindings.startPath);
    }

    private static class Events {

        @SubscribeEvent
        public void onPreClientTick(ClientTickEvent.Pre event) {
            FreeCam.instance.onClientTickStart();
        }

        @SubscribeEvent
        public void onPostClientTick(ClientTickEvent.Post event) {
            ChatCommandManager.instance.onClientTickEnd();
        }
    }

    private static class Bridge implements ModLoaderBridge {

        @Override
        public WrappedRegistry<Block> getBlockRegistry() {
            return new BuiltInWrappedRegistry<>(BuiltInRegistries.BLOCK);
        }
    }

    private record BuiltInWrappedRegistry<T>(DefaultedRegistry<T> registry) implements WrappedRegistry<T> {

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
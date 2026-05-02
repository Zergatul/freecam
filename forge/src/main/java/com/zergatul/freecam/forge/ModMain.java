package com.zergatul.freecam.forge;

import com.zergatul.freecam.*;
import com.zergatul.freecam.ui.FreeCamSettingsScreen;
import net.minecraft.resources.Identifier;
import net.minecraft.world.level.block.Block;
import net.minecraftforge.client.ConfigScreenHandler;
import net.minecraftforge.client.event.RegisterKeyMappingsEvent;
import net.minecraftforge.event.TickEvent;
import net.minecraftforge.eventbus.api.bus.BusGroup;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.minecraftforge.registries.ForgeRegistries;
import net.minecraftforge.registries.IForgeRegistry;

@SuppressWarnings("unused")
@Mod("zergatulfreecam")
public class ModMain {

    public ModMain(FMLJavaModLoadingContext context) {
        BusGroup modBusGroup = context.getModBusGroup();

        FMLCommonSetupEvent.getBus(modBusGroup).addListener(this::setup);
        RegisterKeyMappingsEvent.BUS.addListener(this::setupKeybindings);

        DebugScreenExtensions.register();
        context.registerExtensionPoint(
                ConfigScreenHandler.ConfigScreenFactory.class,
                () -> new ConfigScreenHandler.ConfigScreenFactory(FreeCamSettingsScreen::new));

        ModLoaderBridgeInstance.init(new Bridge());
    }

    private void setup(final FMLCommonSetupEvent event) {
        TickEvent.ClientTickEvent.Pre.BUS.addListener(_ -> FreeCam.instance.onClientTickStart());
        TickEvent.ClientTickEvent.Post.BUS.addListener(_ -> ChatCommandManager.instance.onClientTickEnd());
    }

    private void setupKeybindings(final RegisterKeyMappingsEvent event) {
        event.register(KeyBindings.toggleFreeCam);
        event.register(KeyBindings.toggleCameraLock);
        event.register(KeyBindings.toggleEyeLock);
        event.register(KeyBindings.toggleFollowCam);
        event.register(KeyBindings.startPath);
    }

    private static class Bridge implements ModLoaderBridge {

        @Override
        public WrappedRegistry<Block> getBlockRegistry() {
            return new ForgeWrappedRegistry<>(ForgeRegistries.BLOCKS);
        }
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
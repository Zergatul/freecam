package com.zergatul.freecam;

import net.minecraftforge.client.event.RegisterKeyMappingsEvent;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;

@Mod("freecam")
public class ModMain {

    public ModMain() {
        FMLJavaModLoadingContext.get().getModEventBus().addListener(this::setup);
        FMLJavaModLoadingContext.get().getModEventBus().addListener(this::setupKeybindings);
    }

    private void setup(final FMLCommonSetupEvent event) {
        MinecraftForge.EVENT_BUS.register(ModApiWrapper.instance);
    }

    private void setupKeybindings(final RegisterKeyMappingsEvent event) {
        event.register(KeyBindingsController.toggleFreeCam);
        event.register(KeyBindingsController.toggleCameraLock);
        event.register(KeyBindingsController.toggleEyeLock);
    }
}
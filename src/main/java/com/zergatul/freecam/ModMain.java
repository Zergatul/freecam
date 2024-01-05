package com.zergatul.freecam;

import net.minecraftforge.client.ClientRegistry;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@Mod("freecam")
public class ModMain {

    public ModMain() {
        FMLJavaModLoadingContext.get().getModEventBus().addListener(this::setup);
    }

    private void setup(final FMLCommonSetupEvent event) {
        ClientRegistry.registerKeyBinding(KeyBindingsController.toggleFreeCam);
        ClientRegistry.registerKeyBinding(KeyBindingsController.toggleCameraLock);
        ClientRegistry.registerKeyBinding(KeyBindingsController.toggleEyeLock);
        ClientRegistry.registerKeyBinding(KeyBindingsController.toggleFollowCam);
        MinecraftForge.EVENT_BUS.register(ModApiWrapper.instance);
    }
}

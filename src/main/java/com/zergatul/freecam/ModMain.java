package com.zergatul.freecam;

import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fml.client.registry.ClientRegistry;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.common.event.FMLPreInitializationEvent;

@Mod(modid = "freecam", guiFactory = "com.zergatul.freecam.ui.FreeCamGuiFactory")
public class ModMain {

    @Mod.EventHandler
    public void onPreInit(FMLPreInitializationEvent event) {
        ClientRegistry.registerKeyBinding(KeyBindings.toggleFreeCam);
        ClientRegistry.registerKeyBinding(KeyBindings.toggleCameraLock);
        ClientRegistry.registerKeyBinding(KeyBindings.toggleEyeLock);
        ClientRegistry.registerKeyBinding(KeyBindings.toggleFollowCam);
        MinecraftForge.EVENT_BUS.register(ModApiWrapper.instance);
    }
}
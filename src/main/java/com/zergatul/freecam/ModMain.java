package com.zergatul.freecam;

import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fml.client.registry.ClientRegistry;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.common.event.FMLPreInitializationEvent;

// TODO: itemRenderer.renderItemInFirstPerson

@Mod(modid = "freecam", version = "0.1.0")
public class ModMain {

    @Mod.EventHandler
    public void onPreInit(FMLPreInitializationEvent event) {
        ClientRegistry.registerKeyBinding(KeyBindings.toggleFreeCam);
        MinecraftForge.EVENT_BUS.register(ModApiWrapper.instance);
    }
}
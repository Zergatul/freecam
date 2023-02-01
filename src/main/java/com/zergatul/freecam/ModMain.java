package com.zergatul.freecam;

import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.common.event.FMLPreInitializationEvent;

@Mod(modid = ModMain.MOD_ID, name = "FreeCam by Zergatul", version = "0.1")
public class ModMain {

    public static final String MOD_ID = "freecam";

    @Mod.EventHandler
    public void init(FMLPreInitializationEvent event) {
        KeyBindingsController.instance.setup();
        MinecraftForge.EVENT_BUS.register(ModApiWrapper.instance);
    }
}
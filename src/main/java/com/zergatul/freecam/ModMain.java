package com.zergatul.freecam;

import cpw.mods.fml.common.FMLCommonHandler;
import cpw.mods.fml.common.Mod;
import cpw.mods.fml.common.event.FMLPreInitializationEvent;

@Mod(
        modid = ModMain.MOD_ID,
        name = ModMain.MOD_NAME,
        version = ModMain.VERSION,
        acceptedMinecraftVersions = "[1.7.10]",
        acceptableRemoteVersions = "*",
        guiFactory = "com.zergatul.freecam.ui.FreeCamGuiFactory")
public class ModMain {

    public static final String MOD_ID = "freecam";
    public static final String MOD_NAME = "FreeCam by Zergatul";
    public static final String VERSION = "2.3.0";

    @Mod.EventHandler
    public void preInit(FMLPreInitializationEvent event) {
        ConfigRepository.INSTANCE.init(event.getModConfigurationDirectory());
        KeyBindingsController.INSTANCE.setup();
        FMLCommonHandler.instance().bus().register(ModApiWrapper.INSTANCE);
    }
}
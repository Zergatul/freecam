package com.zergatul.freecam;

import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.common.event.FMLPreInitializationEvent;

@Mod(modid = ModMain.MOD_ID, guiFactory = "com.zergatul.freecam.ui.FreeCamGuiFactory")
public class ModMain {

    public static final String MOD_ID = "freecam";

    @Mod.EventHandler
    public void init(FMLPreInitializationEvent event) {
        ConfigRepository.INSTANCE.init(event.getModConfigurationDirectory());
        KeyBindingsController.INSTANCE.setup();
        MinecraftForge.EVENT_BUS.register(ModApiWrapper.instance);
    }
}
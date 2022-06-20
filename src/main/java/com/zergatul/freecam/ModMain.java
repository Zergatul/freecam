package com.zergatul.freecam;

import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;

@Mod("freecam")
public class ModMain {

    public ModMain() {
        FMLJavaModLoadingContext.get().getModEventBus().addListener(this::setup);
    }

    private void setup(final FMLCommonSetupEvent event) {
        KeyBindingsController.instance.setup();
        MinecraftForge.EVENT_BUS.register(KeyBindingsController.instance);
        MinecraftForge.EVENT_BUS.register(FreeCamController.instance);
    }
}

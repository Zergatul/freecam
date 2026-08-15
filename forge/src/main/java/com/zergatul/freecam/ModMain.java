package com.zergatul.freecam;

import com.zergatul.freecam.ui.FreeCamSettingsScreen;
import net.minecraftforge.client.ConfigScreenHandler;
import net.minecraftforge.client.event.RegisterKeyMappingsEvent;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fml.ModLoadingContext;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;

@Mod("zergatulfreecam")
public class ModMain {

    public ModMain() {
        FMLJavaModLoadingContext.get().getModEventBus().addListener(this::setup);
        FMLJavaModLoadingContext.get().getModEventBus().addListener(this::setupKeybindings);

        ModLoadingContext.get().registerExtensionPoint(
                ConfigScreenHandler.ConfigScreenFactory.class,
                () -> new ConfigScreenHandler.ConfigScreenFactory((mc, screen) -> new FreeCamSettingsScreen(screen)));
    }

    private void setup(final FMLCommonSetupEvent event) {
        MinecraftForge.EVENT_BUS.register(ModApiWrapper.INSTANCE);
    }

    private void setupKeybindings(final RegisterKeyMappingsEvent event) {
        event.register(KeyBindings.toggleFreeCam);
        event.register(KeyBindings.toggleCameraLock);
        event.register(KeyBindings.toggleEyeLock);
        event.register(KeyBindings.toggleFollowCam);
        //event.register(KeyBindings.startPath);
    }
}
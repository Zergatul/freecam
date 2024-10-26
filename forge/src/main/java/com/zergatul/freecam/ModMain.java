package com.zergatul.freecam;

import com.zergatul.freecam.ui.FreeCamSettingsScreen;
import net.minecraftforge.client.ConfigScreenHandler;
import net.minecraftforge.client.event.RegisterKeyMappingsEvent;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fml.IExtensionPoint;
import net.minecraftforge.fml.ModContainer;
import net.minecraftforge.fml.ModLoadingContext;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;

@SuppressWarnings("unused")
@Mod("zergatulfreecam")
public class ModMain {

    public ModMain(FMLJavaModLoadingContext context) {
        context.getModEventBus().addListener(this::setup);
        context.getModEventBus().addListener(this::setupKeybindings);

        context.registerExtensionPoint(
                ConfigScreenHandler.ConfigScreenFactory.class,
                () -> new ConfigScreenHandler.ConfigScreenFactory(FreeCamSettingsScreen::new));
    }

    private void setup(final FMLCommonSetupEvent event) {
        MinecraftForge.EVENT_BUS.register(ModApiWrapper.instance);
    }

    private void setupKeybindings(final RegisterKeyMappingsEvent event) {
        event.register(KeyBindings.toggleFreeCam);
        event.register(KeyBindings.toggleCameraLock);
        event.register(KeyBindings.toggleEyeLock);
        event.register(KeyBindings.toggleFollowCam);
        event.register(KeyBindings.startPath);
    }
}
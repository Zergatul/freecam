package com.zergatul.freecam;

import com.zergatul.freecam.ui.FreeCamSettingsScreen;
import net.minecraftforge.client.ConfigScreenHandler;
import net.minecraftforge.client.event.RegisterKeyMappingsEvent;
import net.minecraftforge.eventbus.api.bus.BusGroup;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;

@SuppressWarnings("unused")
@Mod("zergatulfreecam")
public class ModMain {

    public ModMain(FMLJavaModLoadingContext context) {
        BusGroup modBusGroup = context.getModBusGroup();

        FMLCommonSetupEvent.getBus(modBusGroup).addListener(this::setup);
        RegisterKeyMappingsEvent.BUS.addListener(this::setupKeybindings);

        DebugScreenExtensions.register();
        context.registerExtensionPoint(
                ConfigScreenHandler.ConfigScreenFactory.class,
                () -> new ConfigScreenHandler.ConfigScreenFactory(FreeCamSettingsScreen::new));
    }

    private void setup(final FMLCommonSetupEvent event) {
        ModApiWrapper.instance.init();
    }

    private void setupKeybindings(final RegisterKeyMappingsEvent event) {
        event.register(KeyBindings.toggleFreeCam);
        event.register(KeyBindings.toggleCameraLock);
        event.register(KeyBindings.toggleEyeLock);
        event.register(KeyBindings.toggleFollowCam);
        event.register(KeyBindings.startPath);
    }
}
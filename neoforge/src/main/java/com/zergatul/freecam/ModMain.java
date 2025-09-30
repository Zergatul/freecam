package com.zergatul.freecam;

import com.zergatul.freecam.ui.FreeCamSettingsScreen;
import net.neoforged.neoforge.client.event.RegisterKeyMappingsEvent;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.fml.ModContainer;
import net.neoforged.fml.common.Mod;
import net.neoforged.neoforge.client.gui.IConfigScreenFactory;
import net.neoforged.neoforge.common.NeoForge;

@SuppressWarnings("unused")
@Mod("zergatulfreecam")
public class ModMain {

    public ModMain(IEventBus bus, ModContainer container) {
        bus.addListener(this::onRegisterKeybindings);
        NeoForge.EVENT_BUS.register(ModApiWrapper.instance);
        DebugScreenExtensions.register();
        container.registerExtensionPoint(
                IConfigScreenFactory.class,
                (cont, screen) -> new FreeCamSettingsScreen(screen));
    }

    private void onRegisterKeybindings(final RegisterKeyMappingsEvent event) {
        event.register(KeyBindings.toggleFreeCam);
        event.register(KeyBindings.toggleCameraLock);
        event.register(KeyBindings.toggleEyeLock);
        event.register(KeyBindings.toggleFollowCam);
        event.register(KeyBindings.startPath);
    }
}
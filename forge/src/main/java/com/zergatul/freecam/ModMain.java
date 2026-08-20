package com.zergatul.freecam;

import com.zergatul.freecam.ui.FreeCamSettingsScreen;
import net.minecraftforge.client.ClientRegistry;
import net.minecraftforge.client.ConfigGuiHandler;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.TickEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.ModLoadingContext;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;

@Mod("freecam")
public class ModMain {

    public ModMain() {
        FMLJavaModLoadingContext.get().getModEventBus().addListener(this::setup);
        ModLoadingContext.get().registerExtensionPoint(
                ConfigGuiHandler.ConfigGuiFactory.class,
                () -> new ConfigGuiHandler.ConfigGuiFactory((mc, screen) -> new FreeCamSettingsScreen(screen)));
    }

    private void setup(final FMLCommonSetupEvent event) {
        ClientRegistry.registerKeyBinding(KeyBindings.toggleFreeCam);
        ClientRegistry.registerKeyBinding(KeyBindings.toggleCameraLock);
        ClientRegistry.registerKeyBinding(KeyBindings.toggleEyeLock);
        ClientRegistry.registerKeyBinding(KeyBindings.toggleFollowCam);
        MinecraftForge.EVENT_BUS.register(new Events());
    }

    private static class Events {

        @SubscribeEvent
        public void onRenderTick(TickEvent.RenderTickEvent event) {
            if (event.phase == TickEvent.Phase.START) {
                FreeCam.INSTANCE.onRenderTickStart(event.renderTickTime);
            }
        }

        @SubscribeEvent
        public void onClientTick(TickEvent.ClientTickEvent event) {
            if (event.phase == TickEvent.Phase.START) {
                FreeCam.INSTANCE.onClientTickStart();
            }
        }
    }
}
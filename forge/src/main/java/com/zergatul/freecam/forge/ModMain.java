package com.zergatul.freecam.forge;

import com.zergatul.freecam.Constants;
import com.zergatul.freecam.FreeCam;
import com.zergatul.freecam.KeyBindings;
import com.zergatul.freecam.ui.FreeCamSettingsScreen;
import net.minecraftforge.client.event.ClientChatEvent;
import net.minecraftforge.client.event.InputEvent;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.TickEvent;
import net.minecraftforge.event.world.WorldEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.client.registry.ClientRegistry;
import net.minecraftforge.fml.ExtensionPoint;
import net.minecraftforge.fml.ModLoadingContext;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;

@Mod(Constants.MOD_ID)
public class ModMain {

    public ModMain() {
        FMLJavaModLoadingContext.get().getModEventBus().addListener(this::setup);
        ModLoadingContext.get().registerExtensionPoint(
                ExtensionPoint.CONFIGGUIFACTORY,
                () -> (minecraft, screen) -> new FreeCamSettingsScreen(screen));
    }

    private void setup(final FMLCommonSetupEvent event) {
        ClientRegistry.registerKeyBinding(KeyBindings.TOGGLE_FREE_CAM);
        ClientRegistry.registerKeyBinding(KeyBindings.TOGGLE_CAMERA_LOCK);
        ClientRegistry.registerKeyBinding(KeyBindings.TOGGLE_EYE_LOCK);
        ClientRegistry.registerKeyBinding(KeyBindings.TOGGLE_FOLLOW_CAM);
        MinecraftForge.EVENT_BUS.register(new Events());
    }

    private static class Events {

        @SubscribeEvent
        public void onKeyInputEvent(InputEvent.KeyInputEvent event) {
            FreeCam.INSTANCE.onKeyInput();
        }

        @SubscribeEvent
        public void onClientChatEvent(ClientChatEvent event) {
            if (FreeCam.INSTANCE.onClientChat(event.getMessage())) {
                event.setCanceled(true);
            }
        }

        @SubscribeEvent
        public void onRenderTick(TickEvent.RenderTickEvent event) {
            if (event.phase == TickEvent.Phase.START) {
                FreeCam.INSTANCE.onRenderTickStart();
            }
        }

        @SubscribeEvent
        public void onClientTick(TickEvent.ClientTickEvent event) {
            if (event.phase == TickEvent.Phase.START) {
                FreeCam.INSTANCE.onClientTickStart();
            }
        }

        @SubscribeEvent
        public void onWorldUnload(WorldEvent.Unload event) {
            FreeCam.INSTANCE.onWorldUnload();
        }
    }
}
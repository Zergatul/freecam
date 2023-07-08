package com.zergatul.freecam;

import net.minecraftforge.client.event.ClientChatEvent;
import net.minecraftforge.client.event.RenderHandEvent;
import net.minecraftforge.client.event.RenderWorldLastEvent;
import net.minecraftforge.event.world.WorldEvent;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import net.minecraftforge.fml.common.gameevent.InputEvent;
import net.minecraftforge.fml.common.gameevent.TickEvent;

public class ModApiWrapper {

    public static final ModApiWrapper instance = new ModApiWrapper();

    private ModApiWrapper() {

    }

    @SubscribeEvent
    public void onKeyInputEvent(InputEvent.KeyInputEvent event) {
        FreeCam.instance.onKeyInput();
    }

    @SubscribeEvent
    public void onClientChatEvent(ClientChatEvent event) {
        if (ChatCommandManager.instance.handleChatMessage(event.getMessage())) {
            event.setCanceled(true);
        }
    }

    @SubscribeEvent
    public void onRenderTick(TickEvent.RenderTickEvent event) {
        if (event.phase == TickEvent.Phase.START) {
            FreeCam.instance.onRenderTickStart();
        }
    }

    @SubscribeEvent
    public void onRenderHand(RenderHandEvent event) {
        if (!FreeCam.instance.shouldRenderHands()) {
            event.setCanceled(true);
        }
    }

    @SubscribeEvent
    public void onClientTick(TickEvent.ClientTickEvent event) {
        if (event.phase == TickEvent.Phase.START) {
            FreeCam.instance.onClientTickStart();
        }
    }

    @SubscribeEvent
    public void onWorldUnload(WorldEvent.Unload event) {
        if (event.getWorld().isRemote) {
            FreeCam.instance.onWorldUnload();
        }
    }

    @SubscribeEvent
    public void onRenderWorldLast(RenderWorldLastEvent event) {
        FreeCam.instance.onRenderWorldLast();
    }
}
package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCamController;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.gui.screen.Screen;
import net.minecraft.client.world.ClientWorld;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(MinecraftClient.class)
public abstract class MixinMinecraftClient {

    @Inject(at = @At("HEAD"), method = "Lnet/minecraft/client/MinecraftClient;joinWorld(Lnet/minecraft/client/world/ClientWorld;)V")
    private void onBeforeJoinWorld(ClientWorld world, CallbackInfo info) {
        if (world != null) {
            FreeCamController.instance.onWorldUnload();
        }
    }

    @Inject(at = @At("HEAD"), method = "Lnet/minecraft/client/MinecraftClient;disconnect(Lnet/minecraft/client/gui/screen/Screen;)V")
    private void onBeforeDisconnect(Screen screen, CallbackInfo info) {
        MinecraftClient mc = (MinecraftClient) (Object) this;
        if (mc.world != null) {
            FreeCamController.instance.onWorldUnload();
        }
    }

    @Inject(at = @At("HEAD"), method = "Lnet/minecraft/client/MinecraftClient;render(Z)V")
    private void onBeforeRender(boolean tick, CallbackInfo info) {
        FreeCamController.instance.onRenderTickStart();
    }
}
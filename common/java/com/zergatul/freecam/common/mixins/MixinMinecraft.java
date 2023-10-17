package com.zergatul.freecam.common.mixins;

import com.zergatul.freecam.common.FreeCam;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.screens.Screen;
import net.minecraft.client.multiplayer.ClientLevel;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(Minecraft.class)
public abstract class MixinMinecraft {

    @Inject(at = @At("HEAD"), method = "clearClientLevel(Lnet/minecraft/client/gui/screens/Screen;)V")
    private void onClearClientLevel(Screen screen, CallbackInfo ci) {
        FreeCam.instance.onWorldUnload();
    }

    @Inject(at = @At("HEAD"), method = "setLevel(Lnet/minecraft/client/multiplayer/ClientLevel;)V")
    private void onSetLevel(ClientLevel level, CallbackInfo ci) {
        FreeCam.instance.onWorldUnload();
    }

    @Inject(at = @At("TAIL"), method = "handleKeybinds()V")
    private void onHandleKeyBindings(CallbackInfo info) {
        FreeCam.instance.onHandleKeyBindings();
    }
}
package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCam;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.screens.Screen;
import net.minecraft.client.multiplayer.ClientLevel;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(Minecraft.class)
public abstract class MixinMinecraft {

    @Inject(at = @At("HEAD"), method = "clearLevel(Lnet/minecraft/client/gui/screens/Screen;)V")
    private void onClearLevel(Screen screen, CallbackInfo ci) {
        FreeCam.INSTANCE.onWorldUnload();
    }

    @Inject(at = @At("HEAD"), method = "setLevel(Lnet/minecraft/client/multiplayer/ClientLevel;)V")
    private void onSetLevel(ClientLevel level, CallbackInfo ci) {
        FreeCam.INSTANCE.onWorldUnload();
    }

    @Inject(at = @At("TAIL"), method = "handleKeybinds()V")
    private void onHandleKeyBindings(CallbackInfo info) {
        FreeCam.INSTANCE.onHandleKeyBindings();
    }
}
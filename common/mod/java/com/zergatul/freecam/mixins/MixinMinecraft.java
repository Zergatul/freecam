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

    @Inject(at = @At("HEAD"), method = "clearClientLevel")
    private void onClearClientLevel(Screen screen, CallbackInfo ci) {
        FreeCam.instance.onLevelChange();
    }

    @Inject(at = @At("HEAD"), method = "disconnect(Lnet/minecraft/client/gui/screens/Screen;ZZ)V")
    private void onDisconnect(Screen screen, boolean clearResourcePacks, boolean stopSounds, CallbackInfo ci) {
        FreeCam.instance.onLevelChange();
    }

    @Inject(at = @At("HEAD"), method = "setLevel")
    private void onSetLevel(ClientLevel level, CallbackInfo ci) {
        FreeCam.instance.onLevelChange();
    }

    @Inject(at = @At("TAIL"), method = "handleKeybinds")
    private void onHandleKeyBindings(CallbackInfo info) {
        FreeCam.instance.onHandleKeyBindings();
    }
}
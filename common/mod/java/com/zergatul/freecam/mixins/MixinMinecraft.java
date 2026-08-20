package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCam;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.ClientLevel;
import org.jetbrains.annotations.Nullable;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(Minecraft.class)
public abstract class MixinMinecraft {

    @Shadow
    @Nullable
    public ClientLevel level;

    @Inject(at = @At("TAIL"), method = "handleKeybinds()V")
    private void onHandleKeyBindings(CallbackInfo info) {
        FreeCam.INSTANCE.onHandleKeyBindings();
    }

    @Inject(at = @At("HEAD"), method = "setLevel")
    private void onSetLevel(CallbackInfo info) {
        if (this.level != null) {
            FreeCam.INSTANCE.onWorldUnload();
        }
    }

    @Inject(at = @At("HEAD"), method = "clearLevel(Lnet/minecraft/client/gui/screens/Screen;)V")
    private void onClearLevel(CallbackInfo info) {
        if (this.level != null) {
            FreeCam.INSTANCE.onWorldUnload();
        }
    }
}
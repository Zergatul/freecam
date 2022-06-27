package com.zergatul.freecam.mixins;

import com.zergatul.freecam.helpers.MixinMouseHandlerHelper;
import net.minecraft.client.Mouse;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(Mouse.class)
public abstract class MixinMouse {

    @Inject(at = @At("HEAD"), method = "Lnet/minecraft/client/Mouse;updateMouse()V")
    private void onBeforeUpdateMouse(CallbackInfo info) {
        MixinMouseHandlerHelper.insideUpdateMouse = true;
    }

    @Inject(at = @At("TAIL"), method = "Lnet/minecraft/client/Mouse;updateMouse()V")
    private void onAfterUpdateMouse(CallbackInfo info) {
        MixinMouseHandlerHelper.insideUpdateMouse = false;
    }
}
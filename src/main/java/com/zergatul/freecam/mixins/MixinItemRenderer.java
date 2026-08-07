package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCam;
import net.minecraft.client.renderer.ItemRenderer;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(ItemRenderer.class)
public abstract class MixinItemRenderer {

    @Inject(method = "renderOverlays(F)V", at = @At("HEAD"), cancellable = true, require = 1)
    private void freecam$disableFirstPersonOverlays(float partialTicks, CallbackInfo callback) {
        if (FreeCam.INSTANCE.isActive()) {
            callback.cancel();
        }
    }

    @Inject(method = "renderItemInFirstPerson(F)V", at = @At("HEAD"), require = 1)
    private void freecam$beforeRenderHands(float partialTicks, CallbackInfo callback) {
        FreeCam.INSTANCE.onBeforeRenderHands();
    }

    @Inject(method = "renderItemInFirstPerson(F)V", at = @At("RETURN"), require = 1)
    private void freecam$afterRenderHands(float partialTicks, CallbackInfo callback) {
        FreeCam.INSTANCE.onAfterRenderHands();
    }
}
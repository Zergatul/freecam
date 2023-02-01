package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCamController;
import net.minecraft.client.renderer.EntityRenderer;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
@Mixin(EntityRenderer.class)
public abstract class MixinEntityRenderer {

    @Inject(at = @At("HEAD"), method = "updateCameraAndRender(FJ)V")
    private void onBeforeUpdateCameraAndRender(float partialTicks, long nanoTime, CallbackInfo info) {
        FreeCamController.instance.onBeforeRenderWorld();
    }

    @Inject(at = @At("TAIL"), method = "updateCameraAndRender(FJ)V")
    private void onAfterUpdateCameraAndRender(float partialTicks, long nanoTime, CallbackInfo info) {
        FreeCamController.instance.onAfterRenderWorld();
    }
}
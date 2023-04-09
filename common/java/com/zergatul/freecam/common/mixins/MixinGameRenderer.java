package com.zergatul.freecam.common.mixins;

import com.zergatul.freecam.common.FreeCam;
import net.minecraft.client.CameraType;
import net.minecraft.client.renderer.GameRenderer;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.Redirect;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(GameRenderer.class)
public abstract class MixinGameRenderer {

    @Inject(at = @At("HEAD"), method = "render(FJZ)V")
    private void onRender(float tickDelta, long startTime, boolean tick, CallbackInfo ci) {
        FreeCam.instance.onRenderTickStart(tickDelta);
    }

    @Inject(at = @At("HEAD"), method = "pick(F)V")
    private void onBeforePick(float vec33, CallbackInfo info) {
        FreeCam.instance.onBeforeGameRendererPick();
    }

    @Inject(at = @At("RETURN"), method = "pick(F)V")
    private void onAfterPick(float vec33, CallbackInfo info) {
        FreeCam.instance.onAfterGameRendererPick();
    }

    @Redirect(
            method = "renderItemInHand(Lcom/mojang/blaze3d/vertex/PoseStack;Lnet/minecraft/client/Camera;F)V",
            at = @At(value = "INVOKE", target = "Lnet/minecraft/client/CameraType;isFirstPerson()Z", ordinal = 0))
    private boolean onRenderItemInHandIsFirstPerson(CameraType cameraType) {
        return FreeCam.instance.onRenderItemInHandIsFirstPerson(cameraType);
    }
}
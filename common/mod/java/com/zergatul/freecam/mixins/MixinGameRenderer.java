package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCam;
import net.minecraft.client.Camera;
import net.minecraft.client.CameraType;
import net.minecraft.client.DeltaTracker;
import net.minecraft.client.renderer.GameRenderer;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.Redirect;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(GameRenderer.class)
public abstract class MixinGameRenderer {

    @Inject(at = @At("HEAD"), method = "render")
    private void onRender(DeltaTracker delta, boolean tick, CallbackInfo ci) {
        FreeCam.instance.onRenderTickStart(delta);
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
            method = "renderItemInHand",
            at = @At(value = "INVOKE", target = "Lnet/minecraft/client/CameraType;isFirstPerson()Z", ordinal = 0))
    private boolean onRenderItemInHandIsFirstPerson(CameraType cameraType) {
        return FreeCam.instance.onRenderItemInHandIsFirstPerson(cameraType);
    }

    @Inject(at = @At("HEAD"), method = "getFov", cancellable = true)
    private void onGetFov(Camera camera, float partialTicks, boolean isLevelRender, CallbackInfoReturnable<Float> info) {
        FreeCam.instance.onFovOverride(isLevelRender, info);
    }
}
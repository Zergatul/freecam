package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCam;
import net.minecraft.client.renderer.EntityRenderer;
import net.minecraft.client.settings.GameSettings;
import org.objectweb.asm.Opcodes;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.Redirect;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(EntityRenderer.class)
public abstract class MixinEntityRenderer {

    @Inject(at = @At("HEAD"), method = "updateCameraAndRender(FJ)V")
    private void onBeforeUpdateCameraAndRender(float partialTicks, long nanoTime, CallbackInfo info) {
        FreeCam.instance.onBeforeRenderWorld();
    }

    @Inject(at = @At("TAIL"), method = "updateCameraAndRender(FJ)V")
    private void onAfterUpdateCameraAndRender(float partialTicks, long nanoTime, CallbackInfo info) {
        FreeCam.instance.onAfterRenderWorld();
    }

    @Inject(
            at = @At(value = "INVOKE", target = "Lnet/minecraft/client/renderer/EntityRenderer;getMouseOver(F)V", shift = At.Shift.BEFORE),
            method = "renderWorld(FJ)V")
    private void onBeforeCalcHitResult(float partialTicks, long finishTimeNano, CallbackInfo info) {
        FreeCam.instance.onRenderWorldBeforeCalcHitResult();
    }

    @Inject(
            at = @At(value = "INVOKE", target = "Lnet/minecraft/client/renderer/EntityRenderer;getMouseOver(F)V", shift = At.Shift.AFTER),
            method = "renderWorld(FJ)V")
    private void onAfterCalcHitResult(float partialTicks, long finishTimeNano, CallbackInfo info) {
        FreeCam.instance.onRenderWorldAfterCalcHitResult();
    }

    @Redirect(
            method = "setupCameraTransform",
            at = @At(value = "FIELD", target = "Lnet/minecraft/client/settings/GameSettings;viewBobbing:Z", opcode = Opcodes.GETFIELD))
    private boolean onGetViewBobbing(GameSettings settings) {
        if (FreeCam.instance.shouldDisableBobbing()) {
            return false;
        } else {
            return settings.viewBobbing;
        }
    }
}
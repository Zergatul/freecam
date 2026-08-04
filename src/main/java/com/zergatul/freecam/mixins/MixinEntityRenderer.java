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
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(EntityRenderer.class)
public abstract class MixinEntityRenderer {

    @Inject(
            method = "updateCameraAndRender(FJ)V",
            at = @At(
                    value = "INVOKE",
                    target = "Lnet/minecraft/client/renderer/EntityRenderer;renderWorld(FJ)V"))
    private void onBeforeUpdateCameraAndRender(float partialTicks, long nanoTime, CallbackInfo info) {
        FreeCam.INSTANCE.onBeforeRenderWorld();
    }

    @Inject(at = @At("TAIL"), method = "updateCameraAndRender(FJ)V")
    private void onAfterUpdateCameraAndRender(float partialTicks, long nanoTime, CallbackInfo info) {
        FreeCam.INSTANCE.onAfterRenderWorld();
    }

    @Inject(at = @At("HEAD"), method = "getMouseOver(F)V")
    private void onBeforeGetMouseOver(float partialTicks, CallbackInfo info) {
        FreeCam.INSTANCE.onBeforePick();
    }

    @Inject(at = @At("RETURN"), method = "getMouseOver(F)V")
    private void onAfterGetMouseOver(float partialTicks, CallbackInfo info) {
        FreeCam.INSTANCE.onAfterPick();
    }

    @Inject(at = @At("HEAD"), method = "setupViewBobbing(F)V", cancellable = true)
    private void onSetupViewBobbing(float partialTicks, CallbackInfo info) {
        if (FreeCam.INSTANCE.isActive()) {
            info.cancel();
        }
    }

    @Redirect(
            method = "renderHand(FI)V",
            at = @At(
                    value = "FIELD",
                    target = "Lnet/minecraft/client/settings/GameSettings;thirdPersonView:I",
                    opcode = Opcodes.GETFIELD,
                    ordinal = 0))
    private int onRenderItemInHandGetThirdPersonView(GameSettings settings) {
        if (FreeCam.INSTANCE.isActive()) {
            return FreeCam.INSTANCE.shouldRenderHands() ? 0 : 1;
        }

        return settings.thirdPersonView;
    }
}
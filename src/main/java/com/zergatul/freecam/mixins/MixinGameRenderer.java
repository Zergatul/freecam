package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCamController;
import net.minecraft.client.CameraType;
import net.minecraft.client.Options;
import net.minecraft.client.renderer.GameRenderer;
import org.objectweb.asm.Opcodes;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.Redirect;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(GameRenderer.class)
public abstract class MixinGameRenderer {

    @Inject(at = @At("HEAD"), method = "pick(F)V")
    private void onBeforePick(float vec33, CallbackInfo info) {
        FreeCamController.instance.onBeforeGameRendererPick();
    }

    @Inject(at = @At("RETURN"), method = "pick(F)V")
    private void onAfterPick(float vec33, CallbackInfo info) {
        FreeCamController.instance.onAfterGameRendererPick();
    }

    @Redirect(
            method = "renderItemInHand(Lcom/mojang/blaze3d/vertex/PoseStack;Lnet/minecraft/client/Camera;F)V",
            at = @At(value = "INVOKE", target = "Lnet/minecraft/client/CameraType;isFirstPerson()Z", ordinal = 0))
    private boolean onRenderItemInHandIsFirstPerson(CameraType cameraType) {
        return FreeCamController.instance.onRenderItemInHandIsFirstPerson(cameraType);
    }

    @Redirect(
            method = "renderLevel(FJLcom/mojang/blaze3d/vertex/PoseStack;)V",
            at = @At(value = "FIELD", target = "Lnet/minecraft/client/Options;bobView:Z", opcode = Opcodes.GETFIELD))
    private boolean onRenderLevelGetBobView(Options options) {
        return FreeCamController.instance.getBobView(options);
    }

    @Redirect(
            method = "renderItemInHand(Lcom/mojang/blaze3d/vertex/PoseStack;Lnet/minecraft/client/Camera;F)V",
            at = @At(value = "FIELD", target = "Lnet/minecraft/client/Options;bobView:Z", opcode = Opcodes.GETFIELD))
    private boolean onRenderItemInHandGetBobView(Options options) {
        return FreeCamController.instance.getBobView(options);
    }
}
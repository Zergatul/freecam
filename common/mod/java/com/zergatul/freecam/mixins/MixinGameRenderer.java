package com.zergatul.freecam.mixins;

import com.llamalad7.mixinextras.injector.ModifyExpressionValue;
import com.zergatul.freecam.FreeCam;
import net.minecraft.client.CameraType;
import net.minecraft.client.renderer.GameRenderer;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.Redirect;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(GameRenderer.class)
public abstract class MixinGameRenderer {

    @Inject(at = @At("HEAD"), method = "render")
    private void onRender(CallbackInfo ci) {
        FreeCam.instance.onRenderTickStart();
    }

    @Redirect(
            method = "renderItemInHand",
            at = @At(value = "INVOKE", target = "Lnet/minecraft/client/CameraType;isFirstPerson()Z", ordinal = 0))
    private boolean onRenderItemInHandIsFirstPerson(CameraType cameraType) {
        return FreeCam.instance.onRenderItemInHandIsFirstPerson(cameraType);
    }

    @ModifyExpressionValue(
            method = "render3dHud",
            at = @At(value = "INVOKE", target = "Lnet/minecraft/client/CameraType;isFirstPerson()Z"))
    private boolean onModifyIsFirstPerson3dCrosshair(boolean isFirstPerson) {
        // maybe need priority=2000 like in another call site
        return FreeCam.instance.onRenderCrosshairModifyIsFirstPerson(isFirstPerson);
    }
}
package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCam;
import net.minecraft.client.Camera;
import net.minecraft.client.Minecraft;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(Camera.class)
public abstract class MixinCamera {

    @Shadow
    @Final
    private Minecraft minecraft;

    @Shadow
    private boolean detached;

    @Shadow
    private boolean isPanoramicMode;

    @Shadow(aliases = "Lnet/minecraft/client/Camera;setRotation(FF)V")
    protected abstract void setRotation(final float yRot, final float xRot);

    @Shadow(aliases = "Lnet/minecraft/client/Camera;setPosition(DDD)V")
    protected abstract void setPosition(final double x, final double y, final double z);

    @Inject(
            at = @At(value = "INVOKE", target = "Lnet/minecraft/world/entity/Entity;isPassenger()Z", ordinal = 0),
            method = "alignWithEntity",
            cancellable = true)
    private void onAlignWithEntity(float partialTicks, CallbackInfo info) {
        FreeCam freeCam = FreeCam.instance;
        if (freeCam.isActive()) {
            this.detached = true;
            setRotation(freeCam.getYRot(), freeCam.getXRot());
            setPosition(freeCam.getX(), freeCam.getY(), freeCam.getZ());
            info.cancel();
        }
    }

    @Inject(at = @At("HEAD"), method = "calculateFov", cancellable = true)
    private void onBeforeCalculateFov(float partialTicks, CallbackInfoReturnable<Float> info) {
        if (this.isPanoramicMode) {
            return;
        }

        if (FreeCam.instance.isActive()) {
            info.setReturnValue((float) this.minecraft.options.fov().get());
        }
    }

    @Inject(at = @At("HEAD"), method = "modifyFovBasedOnDeathOrFluid", cancellable = true)
    private void onModifyFovBasedOnDeathOrFluid(float partialTicks, float fov, CallbackInfoReturnable<Float> info) {
        if (FreeCam.instance.isActive()) {
            info.setReturnValue(fov);
        }
    }
}
package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCamController;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.Vec3d;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(Entity.class)
public abstract class MixinEntity {

    @Shadow(aliases = "Lnet/minecraft/entity/Entity;getRotationVector(FF)Lnet/minecraft/util/math/Vec3d;")
    protected abstract Vec3d getRotationVector(float pitch, float yaw);

    @Inject(at = @At("HEAD"), method = "getCameraPosVec(F)Lnet/minecraft/util/math/Vec3d;", cancellable = true)
    private void onGetCameraPosVec(float p_20300_, CallbackInfoReturnable<Vec3d> info) {
        FreeCamController freeCam = FreeCamController.instance;
        if (freeCam.shouldOverrideCameraEntityPosition((Entity) (Object) this)) {
            info.setReturnValue(new Vec3d(freeCam.getX(), freeCam.getY(), freeCam.getZ()));
        }
    }

    @Inject(at = @At("HEAD"), method = "getRotationVec(F)Lnet/minecraft/util/math/Vec3d;", cancellable = true)
    private void onGetRotationVec(float p_20253_, CallbackInfoReturnable<Vec3d> info) {
        FreeCamController freeCam = FreeCamController.instance;
        if (freeCam.shouldOverrideCameraEntityPosition((Entity) (Object) this)) {
            info.setReturnValue(this.getRotationVector(freeCam.getXRot(), freeCam.getYRot()));
        }
    }
}
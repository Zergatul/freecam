package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCam;
import net.minecraft.entity.Entity;
import net.minecraft.util.AxisAlignedBB;
import net.minecraft.util.Vec3;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(Entity.class)
public abstract class MixinEntity {

    @Inject(at = @At("HEAD"), method = "getPositionEyes(F)Lnet/minecraft/util/Vec3;", cancellable = true)
    private void onGetPositionEyes(float partialTicks, CallbackInfoReturnable<Vec3> info) {
        FreeCam freeCam = FreeCam.INSTANCE;
        if (freeCam.shouldOverrideCameraEntityForPicking((Entity) (Object) this)) {
            info.setReturnValue(new Vec3(freeCam.getX(), freeCam.getY(), freeCam.getZ()));
        }
    }

    @Inject(at = @At("HEAD"), method = "getLook(F)Lnet/minecraft/util/Vec3;", cancellable = true)
    private void onGetLook(float partialTicks, CallbackInfoReturnable<Vec3> info) {
        FreeCam freeCam = FreeCam.INSTANCE;
        if (freeCam.shouldOverrideCameraEntityForPicking((Entity) (Object) this)) {
            info.setReturnValue(freeCam.getTargetLookVector());
        }
    }

    @Inject(at = @At("RETURN"), method = "getEntityBoundingBox()Lnet/minecraft/util/AxisAlignedBB;", cancellable = true)
    private void onGetEntityBoundingBox(CallbackInfoReturnable<AxisAlignedBB> info) {
        info.setReturnValue(FreeCam.INSTANCE.getTargetSearchBox((Entity) (Object) this, info.getReturnValue()));
    }
}
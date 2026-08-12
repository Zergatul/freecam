package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCam;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.vector.Vector3d;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(Entity.class)
public abstract class MixinEntity {

    @Shadow(aliases = "Lnet/minecraft/entity/Entity;calculateViewVector(FF)Lnet/minecraft/util/math/vector/Vector3d;")
    protected abstract Vector3d calculateViewVector(float p_20172_, float p_20173_);

    @Inject(at = @At("HEAD"), method = "getEyePosition", cancellable = true)
    private void onGetEyePosition(float p_174824_1_, CallbackInfoReturnable<Vector3d> info) {
        FreeCam freeCam = FreeCam.INSTANCE;
        Entity entity = (Entity) (Object) this;
        if (freeCam.shouldOverrideCameraEntityPosition(entity)) {
            info.setReturnValue(new Vector3d(freeCam.getX(), freeCam.getY(), freeCam.getZ()));
            info.cancel();
        }
    }

    @Inject(at = @At("HEAD"), method = "getViewVector", cancellable = true)
    private void onGetViewVector(float p_20253_, CallbackInfoReturnable<Vector3d> info) {
        FreeCam freeCam = FreeCam.INSTANCE;
        Entity entity = (Entity) (Object) this;
        if (freeCam.shouldOverrideCameraEntityPosition(entity)) {
            info.setReturnValue(this.calculateViewVector(freeCam.getXRot(), freeCam.getYRot()));
            info.cancel();
        }
    }

    @Inject(at = @At("RETURN"), method = "getBoundingBox", cancellable = true)
    private void onGetBoundingBox(CallbackInfoReturnable<AxisAlignedBB> info) {
        info.setReturnValue(FreeCam.INSTANCE.getTargetSearchBox((Entity) (Object) this, info.getReturnValue()));
    }
}
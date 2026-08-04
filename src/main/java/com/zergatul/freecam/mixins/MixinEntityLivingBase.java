package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCam;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.util.Vec3;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(EntityLivingBase.class)
public abstract class MixinEntityLivingBase {

    @Inject(at = @At("HEAD"), method = "getLook(F)Lnet/minecraft/util/Vec3;", cancellable = true)
    private void onGetLook(float partialTicks, CallbackInfoReturnable<Vec3> info) {
        FreeCam freeCam = FreeCam.INSTANCE;
        if (freeCam.shouldOverrideCameraEntityForPicking((EntityLivingBase) (Object) this)) {
            info.setReturnValue(freeCam.getTargetLookVector());
        }
    }
}
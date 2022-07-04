package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCamController;
import com.zergatul.freecam.helpers.MixinMouseHandlerHelper;
import net.minecraft.client.entity.player.ClientPlayerEntity;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.vector.Vector3d;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(Entity.class)
public abstract class MixinEntity {

    @Shadow(aliases = "Lnet/minecraft/entity/Entity;calculateViewVector(FF)Lnet/minecraft/util/math/vector/Vector3d;")
    protected abstract Vector3d calculateViewVector(float p_20172_, float p_20173_);

    @Inject(at = @At("HEAD"), method = "Lnet/minecraft/entity/Entity;turn(DD)V", cancellable = true)
    private void onTurn(double yRot, double xRot, CallbackInfo info) {
        if (!MixinMouseHandlerHelper.insideTurnPlayer) {
            return;
        }
        if (!FreeCamController.instance.isActive()) {
            return;
        }
        Entity entity = (Entity) (Object) this;
        if (entity instanceof ClientPlayerEntity) {
            FreeCamController.instance.onMouseTurn(yRot, xRot);
            info.cancel();
        }
    }

    @Inject(at = @At("HEAD"), method = "Lnet/minecraft/entity/Entity;getEyePosition(F)Lnet/minecraft/util/math/vector/Vector3d;", cancellable = true)
    private void onGetEyePosition(float p_174824_1_, CallbackInfoReturnable<Vector3d> info) {
        FreeCamController freeCam = FreeCamController.instance;
        if (freeCam.isActive() && freeCam.shouldOverridePlayerPosition()) {
            Entity entity = (Entity) (Object) this;
            if (entity instanceof ClientPlayerEntity) {
                info.setReturnValue(new Vector3d(freeCam.getX(), freeCam.getY(), freeCam.getZ()));
                info.cancel();
            }
        }
    }

    @Inject(at = @At("HEAD"), method = "Lnet/minecraft/entity/Entity;getViewVector(F)Lnet/minecraft/util/math/vector/Vector3d;", cancellable = true)
    private void onGetViewVector(float p_20253_, CallbackInfoReturnable<Vector3d> info) {
        FreeCamController freeCam = FreeCamController.instance;
        if (freeCam.isActive() && freeCam.shouldOverridePlayerPosition()) {
            Entity entity = (Entity) (Object) this;
            if (entity instanceof ClientPlayerEntity) {
                info.setReturnValue(this.calculateViewVector(freeCam.getXRot(), freeCam.getYRot()));
                info.cancel();
            }
        }
    }
}
package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCamController;
import com.zergatul.freecam.helpers.MixinGameRendererHelper;
import com.zergatul.freecam.helpers.MixinMouseHandlerHelper;
import net.minecraft.client.player.LocalPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.phys.Vec3;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(Entity.class)
public abstract class MixinEntity {

    @Shadow
    protected abstract Vec3 calculateViewVector(float p_20172_, float p_20173_);

    @Inject(at = @At("HEAD"), method = "Lnet/minecraft/world/entity/Entity;turn(DD)V", cancellable = true)
    private void onTurn(double yRot, double xRot, CallbackInfo info) {
        if (!MixinMouseHandlerHelper.insideTurnPlayer) {
            return;
        }
        if (!FreeCamController.instance.isActive()) {
            return;
        }
        var entity = (Entity) (Object) this;
        if (entity instanceof LocalPlayer) {
            FreeCamController.instance.onMouseTurn(yRot, xRot);
            info.cancel();
        }
    }

    @Inject(at = @At("HEAD"), method = "Lnet/minecraft/world/entity/Entity;getEyePosition(F)Lnet/minecraft/world/phys/Vec3;", cancellable = true)
    private void onGetEyePosition(float p_20300_, CallbackInfoReturnable<Vec3> info) {
        if (!MixinGameRendererHelper.insidePick) {
            return;
        }
        FreeCamController freecam = FreeCamController.instance;
        if (!freecam.isActive()) {
            return;
        }
        var entity = (Entity) (Object) this;
        if (entity instanceof LocalPlayer) {
            info.setReturnValue(new Vec3(freecam.getX(), freecam.getY(), freecam.getZ()));
            info.cancel();
        }
    }

    @Inject(at = @At("HEAD"), method = "Lnet/minecraft/world/entity/Entity;getViewVector(F)Lnet/minecraft/world/phys/Vec3;", cancellable = true)
    private void onGetViewVector(float p_20253_, CallbackInfoReturnable<Vec3> info) {
        if (!MixinGameRendererHelper.insidePick) {
            return;
        }
        FreeCamController freecam = FreeCamController.instance;
        if (!freecam.isActive()) {
            return;
        }
        var entity = (Entity) (Object) this;
        if (entity instanceof LocalPlayer) {
            info.setReturnValue(this.calculateViewVector(freecam.getXRot(), freecam.getYRot()));
            info.cancel();
        }
    }
}
package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCam;
import net.minecraft.client.renderer.EntityRenderer;
import net.minecraft.client.settings.GameSettings;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.util.AxisAlignedBB;
import org.objectweb.asm.Opcodes;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.Redirect;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(EntityRenderer.class)
public abstract class MixinEntityRenderer {

    @Inject(method = "renderWorld(FJ)V", at = @At("HEAD"), require = 1)
    private void freecam$beforeRenderWorld(float partialTicks, long finishTimeNano, CallbackInfo callback) {
        FreeCam.INSTANCE.onBeforeRenderWorld();
    }

    @Inject(method = "renderWorld(FJ)V", at = @At("RETURN"), require = 1)
    private void freecam$afterRenderWorld(float partialTicks, long finishTimeNano, CallbackInfo callback) {
        FreeCam.INSTANCE.onAfterRenderWorld();
    }

    @Inject(method = "getMouseOver(F)V", at = @At("HEAD"), require = 1)
    private void freecam$beforeGetMouseOver(float partialTicks, CallbackInfo callback) {
        FreeCam.INSTANCE.onBeforePick();
    }

    @Inject(method = "getMouseOver(F)V", at = @At("RETURN"), require = 1)
    private void freecam$afterGetMouseOver(float partialTicks, CallbackInfo callback) {
        FreeCam.INSTANCE.onAfterPick();
    }

    @Redirect(
            method = "getMouseOver(F)V",
            at = @At(
                    value = "FIELD",
                    target = "Lnet/minecraft/entity/EntityLivingBase;boundingBox:Lnet/minecraft/util/AxisAlignedBB;",
                    opcode = Opcodes.GETFIELD),
            require = 1)
    private AxisAlignedBB freecam$getCameraBoundingBox(EntityLivingBase entity) {
        return FreeCam.INSTANCE.getTargetSearchBox(entity, entity.boundingBox);
    }

    @Inject(method = "setupViewBobbing(F)V", at = @At("HEAD"), cancellable = true, require = 1)
    private void freecam$disableViewBobbing(float partialTicks, CallbackInfo callback) {
        if (FreeCam.INSTANCE.isActive()) {
            callback.cancel();
        }
    }

    @Redirect(
            method = "renderHand(FI)V",
            at = @At(
                    value = "FIELD",
                    target = "Lnet/minecraft/client/settings/GameSettings;thirdPersonView:I",
                    opcode = Opcodes.GETFIELD,
                    ordinal = 0),
            require = 1)
    private int freecam$getPerspectiveForHand(GameSettings settings) {
        if (FreeCam.INSTANCE.isActive()) {
            return FreeCam.INSTANCE.shouldRenderHands() ? 0 : 1;
        }
        return settings.thirdPersonView;
    }
}
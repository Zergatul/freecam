package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCam;
import net.minecraft.client.gui.screens.inventory.InventoryScreen;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(InventoryScreen.class)
public abstract class MixinInventoryScreen {

    @Inject(
            method = "renderEntityInInventory",
            at = @At(value = "INVOKE", target = "Lcom/mojang/blaze3d/systems/RenderSystem;runAsFancy(Ljava/lang/Runnable;)V"))
    private static void onBeforeRenderEntityInInventory(CallbackInfo info) {
        FreeCam.INSTANCE.onBeforeRenderPlayerInInventory();
    }

    @Inject(
            method = "renderEntityInInventory",
            at = @At(value = "INVOKE", target = "Lcom/mojang/blaze3d/systems/RenderSystem;runAsFancy(Ljava/lang/Runnable;)V", shift = At.Shift.AFTER))
    private static void onAfterRenderEntityInInventory(CallbackInfo info) {
        FreeCam.INSTANCE.onAfterRenderPlayerInInventory();
    }
}
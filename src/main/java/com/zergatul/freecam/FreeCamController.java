package com.zergatul.freecam;

import com.mojang.authlib.GameProfile;
import com.zergatul.freecam.interfaces.CameraMixinInterface;
import com.zergatul.freecam.interfaces.ClientboundPlayerInfoPacketMixinInterface;
import net.minecraft.client.Minecraft;
import net.minecraft.client.player.*;
import net.minecraft.core.NonNullList;
import net.minecraft.network.protocol.Packet;
import net.minecraft.network.protocol.game.*;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.player.Abilities;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.GameType;
import net.minecraftforge.event.world.WorldEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.*;

public class FreeCamController {

    public static final FreeCamController instance = new FreeCamController();

    private static final int fakePlayerEntityId = Integer.MIN_VALUE;
    private static final UUID fakePlayerProfileUUID = new UUID(0x1234567812345678L, 0x1234567812345678L);
    private static final String fakePlayerName = "FakePlayerName";
    private static final GameProfile fakeProfile = new GameProfile(fakePlayerProfileUUID, fakePlayerName);
    private static final UUID shadowCopyPlayerProfileUUID = new UUID(0x1234567812345678L, 0x1234567812345679L);
    private static final int shadowCopyPlayerEntityId = Integer.MIN_VALUE + 1;

    public GameProfile profileOverride;

    private boolean active = false;
    private Minecraft mc = Minecraft.getInstance();
    private LocalPlayer player;
    private FakePlayer fake;
    private ShadowCopyPlayer shadow;
    private GameType oldGameType;
    private Input oldInput;
    private Abilities oldAbilities;
    private Set<Packet<?>> dontSkip = new HashSet<>();
    private final Logger logger = LogManager.getLogger(FreeCamController.class);

    private FreeCamController() {
        NetworkPacketsController.instance.addServerPacketHandler(this::onServerPacket);
        NetworkPacketsController.instance.addClientPacketHandler(this::onClientPacket);
    }

    public void toggle() {
        synchronized (this) {
            active = !active;
            if (active) {
                enable();
            } else {
                disable();
            }
        }
    }

    public boolean isActive() {
        return active;
    }

    @SubscribeEvent
    public void onWorldUnload(WorldEvent.Unload event) {
        if (active) {
            toggle();
        }
    }

    private void enable() {

        player = mc.player;
        oldInput = player.input;
        player.input = new FakeInput();
        saveAbilities();

        profileOverride = fakeProfile;
        try {
            fake = new FakePlayer(mc);
        } finally {
            profileOverride = null;
        }

        mc.level.addPlayer(fakePlayerEntityId, fake);

        fake.setPos(mc.player.getX(), mc.player.getY(), mc.player.getZ());
        fake.absMoveTo(mc.player.getX(), mc.player.getY(), mc.player.getZ(), mc.player.getYRot(), mc.player.getXRot());
        fake.setDeltaMovement(0, 0, 0);

        mc.player = fake;
        mc.cameraEntity = fake;
        ((CameraMixinInterface) mc.gameRenderer.getMainCamera()).setEntity(fake);

        oldGameType = mc.gameMode.getPlayerMode();
        mc.gameMode.setLocalMode(GameType.SPECTATOR);

        shadow = new ShadowCopyPlayer(player);
        mc.level.addPlayer(shadowCopyPlayerEntityId, shadow);

        var packet = new ClientboundPlayerInfoPacket(ClientboundPlayerInfoPacket.Action.ADD_PLAYER);
        var list = new ArrayList<ClientboundPlayerInfoPacket.PlayerUpdate>();
        list.add(new ClientboundPlayerInfoPacket.PlayerUpdate(fakeProfile, 0, GameType.SPECTATOR, null));
        ((ClientboundPlayerInfoPacketMixinInterface) packet).setEntries(list);
        player.connection.handlePlayerInfo(packet);

    }

    private void disable() {

        player.input = oldInput;
        restoreAbilities();

        mc.player = player;
        mc.cameraEntity = player;
        ((CameraMixinInterface) mc.gameRenderer.getMainCamera()).setEntity(player);

        if (mc.gameMode != null) {
            mc.gameMode.setLocalMode(oldGameType);
        }

        removeClientPlayer(fake);
        removeClientPlayer(shadow);
    }

    private void saveAbilities() {
        var abilities = player.getAbilities();
        oldAbilities = new Abilities();
        oldAbilities.mayfly = abilities.mayfly;
        oldAbilities.flying = abilities.flying;
        oldAbilities.instabuild = abilities.instabuild;
        oldAbilities.invulnerable = abilities.invulnerable;
        oldAbilities.mayBuild = abilities.mayfly;
        oldAbilities.setWalkingSpeed(abilities.getWalkingSpeed());
        oldAbilities.setFlyingSpeed(abilities.getFlyingSpeed());
    }

    private void restoreAbilities() {
        var abilities = player.getAbilities();
        abilities.mayfly = oldAbilities.mayfly;
        abilities.flying = oldAbilities.flying;
        abilities.instabuild = oldAbilities.instabuild;
        abilities.invulnerable = oldAbilities.invulnerable;
        abilities.mayBuild = oldAbilities.mayfly;
        abilities.setWalkingSpeed(oldAbilities.getWalkingSpeed());
        abilities.setFlyingSpeed(oldAbilities.getFlyingSpeed());
    }

    private void removeClientPlayer(AbstractClientPlayer clientPlayer) {
        clientPlayer.remove(Entity.RemovalReason.DISCARDED);

        var packet = new ClientboundPlayerInfoPacket(ClientboundPlayerInfoPacket.Action.REMOVE_PLAYER);
        var list = new ArrayList<ClientboundPlayerInfoPacket.PlayerUpdate>();
        list.add(new ClientboundPlayerInfoPacket.PlayerUpdate(clientPlayer.getGameProfile(), 0, GameType.DEFAULT_MODE, null));
        ((ClientboundPlayerInfoPacketMixinInterface) packet).setEntries(list);
        player.connection.handlePlayerInfo(packet);
    }

    private void onServerPacket(NetworkPacketsController.ServerPacketArgs args) {

        if (active) {
            if (args.packet instanceof ClientboundPlayerPositionPacket) {
                /*SPlayerPositionLookPacket packet = (SPlayerPositionLookPacket)args.packet;
                handlePlayerPositionLook(packet);
                NetworkPacketsController.instance.sendPacket(new CConfirmTeleportPacket(packet.getId()));
                CPlayerPacket.PositionRotationPacket posRotPacket = new CPlayerPacket.PositionRotationPacket(packet.getX(), packet.getY(), packet.getZ(), packet.getYRot(), packet.getXRot(), false);
                synchronized (dontSkip) {
                    dontSkip.add(posRotPacket);
                }
                NetworkPacketsController.instance.sendPacket(posRotPacket);
                args.skip = true;*/
                return;
            }
            /*if (args.packet instanceof SChangeGameStatePacket) {
                args.skip = true;
                return;
            }*/
            /*if (args.packet instanceof ClientboundPlayerAbilitiesPacket) {
                ClientboundPlayerAbilitiesPacket packet = (ClientboundPlayerAbilitiesPacket) args.packet;
                fake.getAbilities().flying = packet.isFlying();
                fake.getAbilities().instabuild = packet.canInstabuild();
                fake.getAbilities().invulnerable = packet.isInvulnerable();
                fake.getAbilities().mayfly = packet.canFly();
                fake.getAbilities().setFlyingSpeed(packet.getFlyingSpeed());
                fake.getAbilities().setWalkingSpeed(packet.getWalkingSpeed());
                args.skip = true;
                return;
            }*/
            /*if (args.packet instanceof SEntityVelocityPacket) {
                SEntityVelocityPacket packet = (ClientBoundEntity)args.packet;
                if (mc.player.getId() == packet.getId()) {
                    if (fake != null) {
                        fake.lerpMotion(packet.getXa() / 8000d, packet.getYa() / 8000d, packet.getZa() / 8000d);
                        args.skip = true;
                    }
                }
                return;
            }*/

            /*if (args.packet instanceof ClientboundMoveEntityPacket) {
                ClientboundMoveEntityPacket packet = (ClientboundMoveEntityPacket) args.packet;
                Entity entity = packet.getEntity(mc.level);
                if (entity instanceof LocalPlayer) {
                    ((SEntityPacketMixinInterface)packet).setEntityId(fake.getId());
                }
                return;
            }*/
            if (args.packet instanceof ClientboundEntityEventPacket) {
                ClientboundEntityEventPacket packet = (ClientboundEntityEventPacket) args.packet;
                if (packet.getEntity(this.mc.level) == player && packet.getEventId() != 35 && packet.getEventId() != 21) {
                    if (active) {
                        toggle();
                    }
                }
                return;
            }
        }
    }

    private void onClientPacket(NetworkPacketsController.ClientPacketArgs args) {

        if (active) {
            if (args.packet instanceof ServerboundMovePlayerPacket.StatusOnly) {
                args.packet = new ServerboundMovePlayerPacket.StatusOnly(
                        player.isOnGround());
                return;
            }
            if (args.packet instanceof ServerboundMovePlayerPacket.Rot) {
                args.packet = new ServerboundMovePlayerPacket.Rot(
                        player.getYRot(),
                        player.getXRot(),
                        player.isOnGround());
                return;
            }
            if (args.packet instanceof ServerboundMovePlayerPacket.Pos) {
                args.packet = new ServerboundMovePlayerPacket.Pos(
                        player.getX(),
                        player.getY(),
                        player.getZ(),
                        player.isOnGround());
                return;
            }
            if (args.packet instanceof ServerboundMovePlayerPacket.PosRot) {
                args.packet = new ServerboundMovePlayerPacket.PosRot(
                        player.getX(),
                        player.getY(),
                        player.getZ(),
                        player.getYRot(),
                        player.getXRot(),
                        player.isOnGround());
                return;
            }
            if (args.packet instanceof ServerboundMovePlayerPacket) {
                logger.error("Subclass case is missing {}", args.packet.getClass().getName());
                return;
            }
        }
    }

    public static class FakePlayer extends LocalPlayer {

        public FakePlayer(Minecraft mc) {
            super(mc, mc.player.clientLevel, mc.player.connection, mc.player.getStats(), mc.player.getRecipeBook(), false, false);

            input = new KeyboardInput(mc.options);

            getAbilities().flying = true;
            getAbilities().instabuild = true;
            getAbilities().invulnerable = true;
            getAbilities().mayfly = true;

            uuid = Mth.createInsecureUUID(new Random());
        }

        @Override
        public boolean isSpectator() {
            return true;
        }
    }

    public static class ShadowCopyPlayer extends RemotePlayer {

        public ShadowCopyPlayer(LocalPlayer player) {
            super(player.clientLevel, new GameProfile(shadowCopyPlayerProfileUUID, player.getGameProfile().getName()));

            this.setPos(player.getX(), player.getY(), player.getZ());
            this.setRot(player.getYRot(), player.getXRot());
            this.setYHeadRot(player.getYHeadRot());

            copyItems(player.getInventory().armor, this.getInventory().armor);
            copyItems(player.getInventory().items, this.getInventory().items);
            copyItems(player.getInventory().offhand, this.getInventory().offhand);

            this.getInventory().selected = player.getInventory().selected;
        }

        private static void copyItems(NonNullList<ItemStack> source, NonNullList<ItemStack> destination) {
            for (int i = 0; i < source.size(); i++) {
                destination.set(i, source.get(i));
            }
        }
    }

    private static class FakeInput extends Input {
        @Override
        public void tick(boolean p_108576_) {
            if (p_108576_) {
                this.leftImpulse *= 0.3F;
                this.forwardImpulse *= 0.3F;
            }
        }
    }
}

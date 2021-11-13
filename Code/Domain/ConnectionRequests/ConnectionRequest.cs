﻿using DDDNetCore.Domain.Shared;
using DDDSample1.Domain.Players;
using DDDSample1.Domain.Shared;
using System;
using System.Collections.Generic;

namespace DDDNetCore.Domain.ConnectionRequests
{
    public abstract class ConnectionRequest : Entity<ConnectionRequestId>, IAggregateRoot
    {
        public PlayerId Player { get; private set; }

        public PlayerId Target { get; private set; }

        public Message PlayerToTargetMessage { get; private set; }

        public ConnectionRequestStatus CurrentStatus { get; private set; }

        public bool Active { get; private set; }

        public ConnectionRequest()
        {
            Active = true;
        }

        protected ConnectionRequest(string player, string target, string playerToTargetMessage, string currentStatus)
        {
            Id = new ConnectionRequestId(Guid.NewGuid());
            Player = new PlayerId(player);
            Target = new PlayerId(target);
            PlayerToTargetMessage = new Message(playerToTargetMessage);
            _ = Enum.TryParse(currentStatus, out ConnectionRequestStatusEnum status);
            CurrentStatus = new ConnectionRequestStatus(status);
            Active = true;
        }

        public void ChangePlayer(string player)
        {
            if (!Active)
            {
                throw new BusinessRuleValidationException("It is not possible to change the player of an inactive ConnectionRequest!");
            }
            Player = new PlayerId(player);
        }

        public void ChangeTarget(string target)
        {
            if (!Active)
            {
                throw new BusinessRuleValidationException("It is not possible to change the target of an inactive ConnectionRequest!");
            }
            Target = new PlayerId(target);
        }

        public void ChangePlayerToTargetMessage(string playerToTargetMessage)
        {
            if (!Active)
            {
                throw new BusinessRuleValidationException("It is not possible to change the player to target message of an inactive ConnectionRequest!");
            }
            PlayerToTargetMessage = new Message(playerToTargetMessage);
        }

        public void ChangeCurrentStatus(string currentStatus)
        {
            if (!Active)
            {
                throw new BusinessRuleValidationException("It is not possible to current status of an inactive ConnectionRequest!");
            }
            _ = Enum.TryParse(currentStatus, out ConnectionRequestStatusEnum status);
            CurrentStatus = new ConnectionRequestStatus(status);
        }

        public void MarkAsInactive()
        {
            Active = false;
        }

        public override bool Equals(object obj)
        {
            return obj is ConnectionRequest request &&
                   EqualityComparer<ConnectionRequestId>.Default.Equals(Id, request.Id);
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(Id);
        }
    }
}
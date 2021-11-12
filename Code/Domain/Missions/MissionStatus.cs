using DDDSample1.Domain.Shared;
using System;
using System.ComponentModel.DataAnnotations.Schema;

namespace DDDNetCore.Domain.Missions
{
    public enum MissionStatusEnum
    {
        active,
        canceled,
        completed,
        in_progress,
        suspended
    }

    [ComplexType]
    public class MissionStatus : IValueObject
    {
        public MissionStatusEnum CurrentStatus { get; private set; }

        public MissionStatus(MissionStatusEnum currentStatus)
        {
            CurrentStatus = currentStatus;
        }

        public void ChangeCurrentStatus(MissionStatusEnum currentStatus)
        {
            CurrentStatus = currentStatus;
        }

        public override bool Equals(object obj)
        {
            return obj is MissionStatus status &&
                   CurrentStatus == status.CurrentStatus;
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(CurrentStatus);
        }
    }
}

using DDDSample1.Domain.Shared;
using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace DDDNetCore.Domain.ConnectionRequests
{
    public enum ConnectionRequestStatusEnum
    {
        introduction_pending,
        introduction_refused,
        request_pending,
        request_refused,
        accepted
    }

    [ComplexType]
    public class ConnectionRequestStatus : IValueObject 
    {
        [Required]
        public ConnectionRequestStatusEnum CurrentStatus { get; private set; }

        public ConnectionRequestStatus(ConnectionRequestStatusEnum currentStatus)
        {
            CurrentStatus = currentStatus;
        }

        public void ChangeCurrentStatus(ConnectionRequestStatusEnum currentStatus)
        {
            CurrentStatus = currentStatus;
        }

        public override bool Equals(object obj)
        {
            return obj is ConnectionRequestStatus status &&
                   CurrentStatus == status.CurrentStatus;
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(CurrentStatus);
        }
    }
}

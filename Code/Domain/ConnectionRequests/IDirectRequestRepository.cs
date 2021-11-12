using DDDSample1.Domain.Shared;

namespace DDDNetCore.Domain.ConnectionRequests
{
    public interface IDirectRequestRepository : IRepository<DirectRequest, ConnectionRequestId>
    {
    }
}

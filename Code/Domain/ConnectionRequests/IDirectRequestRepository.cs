using DDDSample1.Domain.Players;
using DDDSample1.Domain.Shared;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.ConnectionRequests
{
    public interface IDirectRequestRepository : IRepository<DirectRequest, ConnectionRequestId>
    {
        Task<List<DirectRequest>> GetAllUserPendingDirectRequestsAsync(PlayerId playerId);
    }
}

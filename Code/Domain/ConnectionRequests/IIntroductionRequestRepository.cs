using DDDSample1.Domain.Players;
using DDDSample1.Domain.Shared;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.ConnectionRequests
{
    public interface IIntroductionRequestRepository : IRepository<IntroductionRequest, ConnectionRequestId>
    {
        Task<List<IntroductionRequest>> GetAllUserPendingIntroductionRequestsAsync(PlayerId playerId);

        Task<IntroductionRequest> GetPendingIntroductionRequestByPlayerIds(PlayerId player, PlayerId target);
        Task<List<Player>> GetReachableUsers(PlayerId playerId);
        Task<List<Player>> GetMiddlemanList(PlayerId playerId, PlayerId targetId);

    }
}

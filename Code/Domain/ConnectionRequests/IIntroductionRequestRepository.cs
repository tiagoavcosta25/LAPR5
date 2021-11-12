using DDDSample1.Domain.Shared;

namespace DDDNetCore.Domain.ConnectionRequests
{
    public interface IIntroductionRequestRepository : IRepository<IntroductionRequest, ConnectionRequestId>
    {
    }
}

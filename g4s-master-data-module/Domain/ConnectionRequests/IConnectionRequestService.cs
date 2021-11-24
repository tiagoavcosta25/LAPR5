using DDDSample1.Domain.Players;
using DDDSample1.Domain.Shared;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDNetCore.Domain.ConnectionRequests.DTOS;

namespace DDDNetCore.Domain.ConnectionRequests
{
    public interface IConnectionRequestService
    {

        Task<List<ConnectionRequestDto>> GetAllAsync();

        Task<ConnectionRequestDto> GetByIdAsync(ConnectionRequestId id);

        Task<DirectRequestDto> AddDirAsync(CreatingDirectRequestDto dto);

        Task<IntroductionRequestDto> AddIntAsync(CreatingIntroductionRequestDto dto);

        Task<DirectRequestDto> UpdateDirAsync(DirectRequestDto dto);

        Task<IntroductionRequestDto> UpdateIntAsync(IntroductionRequestDto dto);

        Task<ConnectionRequestDto> InactivateAsync(ConnectionRequestId id);

        Task<ConnectionRequestDto> DeleteAsync(ConnectionRequestId id);

        Task<List<TargetPendingRequestDto>> GetAllUserPendingDirectRequestsAsync(string email);

        Task<ConnectionRequestDto> GetByEmailsAsync(string playerEmail, string targetEmail);

        Task<ConnectionRequestDto> AcceptRequest(AcceptRequestDto dto);

        Task<List<ListMidPendingRequestDto>> GetAllUserPendingMidRequests(string email);

        Task<DirectRequestDto> AddDirectRequestAsync(CreatingDirectRequestAutoDto dto);

        Task<ApproveRequestDto> ApproveRequest(ConnectionRequestId id, ApproveRequestDto dto);

        Task<List<IntroductionRequestDto>> GetMiddleManRequests(string playerEmail);


    }
}

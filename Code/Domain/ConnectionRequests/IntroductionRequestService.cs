using DDDNetCore.Domain.ConnectionRequests.DTOS;
using DDDSample1.Domain.Players;
using DDDSample1.Domain.Shared;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.ConnectionRequests
{
    public class IntroductionRequestService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IIntroductionRequestRepository _repo;
        private readonly IPlayerRepository _repoPl;

        public IntroductionRequestService(IUnitOfWork unitOfWork, IIntroductionRequestRepository repo, IPlayerRepository repoPl)
        {
            _unitOfWork = unitOfWork;
            _repo = repo;
            _repoPl = repoPl;
        }

        public async Task<List<IntroductionRequestDto>> GetAllAsync()
        {
            var list = await _repo.GetAllAsync();

            List<IntroductionRequestDto> listDto = list.ConvertAll<IntroductionRequestDto>(intr =>
                new IntroductionRequestDto(intr.Id.AsString(), intr.Player.AsString(), intr.MiddleMan.AsString(), intr.Target.AsString(),
                intr.PlayerToTargetMessage.Text, intr.PlayerToMiddleManMessage.Text, intr.MiddleManToTargetMessage.Text, intr.CurrentStatus.ToString()));

            return listDto;
        }

        public async Task<IntroductionRequestDto> GetByIdAsync(ConnectionRequestId id)
        {
            var intr = await _repo.GetByIdAsync(id);

            if (intr == null)
                return null;

            return new IntroductionRequestDto(intr.Id.AsString(), intr.Player.AsString(), intr.MiddleMan.AsString(), intr.Target.AsString(),
                            intr.PlayerToTargetMessage.Text, intr.PlayerToMiddleManMessage.Text, intr.MiddleManToTargetMessage.Text, intr.CurrentStatus.ToString());
        }

        public async Task<IntroductionRequestDto> AddAsync(CreatingIntroductionRequestDto dto)
        {
            await checkPlayerIdAsync(new PlayerId(dto.Player));
            await checkPlayerIdAsync(new PlayerId(dto.MiddleMan));
            await checkPlayerIdAsync(new PlayerId(dto.Target));
            var intr = new IntroductionRequest(dto.Player, dto.Target, dto.PlayerToTargetMessage, dto.CurrentStatus, dto.MiddleMan, dto.PlayerToMiddleManMessage, dto.MiddleManToTargetMessage);

            await _repo.AddAsync(intr);

            await _unitOfWork.CommitAsync();

            return new IntroductionRequestDto(intr.Id.AsString(), intr.Player.AsString(), intr.MiddleMan.AsString(), intr.Target.AsString(),
                            intr.PlayerToTargetMessage.Text, intr.PlayerToMiddleManMessage.Text, intr.MiddleManToTargetMessage.Text, intr.CurrentStatus.ToString());
        }

        public async Task<IntroductionRequestDto> UpdateAsync(IntroductionRequestDto dto)
        {
            await checkPlayerIdAsync(new PlayerId(dto.Player));
            await checkPlayerIdAsync(new PlayerId(dto.MiddleMan));
            await checkPlayerIdAsync(new PlayerId(dto.Target));
            var intr = await _repo.GetByIdAsync(new ConnectionRequestId(dto.Id));

            if (intr == null)
                return null;

            intr.ChangePlayer(dto.Player);
            intr.ChangeMiddleMan(dto.MiddleMan);
            intr.ChangeTarget(dto.Target);
            intr.ChangePlayerToTargetMessage(dto.PlayerToTargetMessage);
            intr.ChangePlayerToMiddleManMessage(dto.PlayerToMiddleManMessage);
            intr.ChangeMiddleManToTargetMessage(dto.MiddleManToTargetMessage);
            intr.ChangeCurrentStatus(dto.CurrentStatus);

            await _unitOfWork.CommitAsync();

            return new IntroductionRequestDto(intr.Id.AsString(), intr.Player.AsString(), intr.MiddleMan.AsString(), intr.Target.AsString(),
                              intr.PlayerToTargetMessage.Text, intr.PlayerToMiddleManMessage.Text, intr.MiddleManToTargetMessage.Text, intr.CurrentStatus.ToString());
        }

        public async Task<IntroductionRequestDto> InactivateAsync(ConnectionRequestId id)
        {
            var intr = await _repo.GetByIdAsync(id);

            if (intr == null)
                return null;

            intr.MarkAsInactive();

            await _unitOfWork.CommitAsync();

            return new IntroductionRequestDto(intr.Id.AsString(), intr.Player.AsString(), intr.MiddleMan.AsString(), intr.Target.AsString(),
                              intr.PlayerToTargetMessage.Text, intr.PlayerToMiddleManMessage.Text, intr.MiddleManToTargetMessage.Text, intr.CurrentStatus.ToString());
        }

        public async Task<IntroductionRequestDto> DeleteAsync(ConnectionRequestId id)
        {
            var intr = await _repo.GetByIdAsync(id);

            if (intr == null)
                return null;

            _repo.Remove(intr);
            await _unitOfWork.CommitAsync();

            return new IntroductionRequestDto(intr.Id.AsString(), intr.Player.AsString(), intr.MiddleMan.AsString(), intr.Target.AsString(),
                              intr.PlayerToTargetMessage.Text, intr.PlayerToMiddleManMessage.Text, intr.MiddleManToTargetMessage.Text, intr.CurrentStatus.ToString());
        }

        private async Task checkPlayerIdAsync(PlayerId playerId)
        {
            var pl = await _repoPl.GetByIdAsync(playerId);
            if (pl == null)
                throw new BusinessRuleValidationException("Invalid Player or Friend Id.");
        }

        private async Task checkPlayerEmailAsync(string playerEmail)
        {
            var pl = await _repoPl.GetByEmailAsync(playerEmail);
            if (pl == null)
                throw new BusinessRuleValidationException("Invalid Player or Friend Email.");
        }


        // CRUD OVER //

    }
}

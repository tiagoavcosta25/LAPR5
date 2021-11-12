using DDDNetCore.Domain.ConnectionRequests.DTOS;
using DDDSample1.Domain.Players;
using DDDSample1.Domain.Shared;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.ConnectionRequests
{
    public class ConnectionRequestService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IDirectRequestRepository _repoDir;
        private readonly IIntroductionRequestRepository _repoInt;
        private readonly IPlayerRepository _repoPl;

        public ConnectionRequestService(IUnitOfWork unitOfWork, IDirectRequestRepository repoDir ,
            IIntroductionRequestRepository repoInt, IPlayerRepository repoPl)
        {
            _unitOfWork = unitOfWork;
            _repoDir = repoDir;
            _repoInt = repoInt;
            _repoPl = repoPl;
        }



        public async Task<ConnectionRequestListDto> GetAllAsync()
        {
            var listInt = await _repoInt.GetAllAsync();
            var listDir = await _repoDir.GetAllAsync();

            List<IntroductionRequestDto> listIntDto = listInt.ConvertAll<IntroductionRequestDto>(intr =>
                new IntroductionRequestDto(intr.Id.AsString(), intr.Player.AsString(), intr.MiddleMan.AsString(), intr.Target.AsString(),
                intr.PlayerToTargetMessage.Text, intr.PlayerToMiddleManMessage.Text, intr.MiddleManToTargetMessage.Text, intr.CurrentStatus.ToString()));


            List<DirectRequestDto> listDirDto = listDir.ConvertAll<DirectRequestDto>(dir =>
                new DirectRequestDto(dir.Id.AsString(), dir.Player.AsString(), dir.Target.AsString(),
                dir.PlayerToTargetMessage.Text, dir.CurrentStatus.ToString()));

            ConnectionRequestListDto dto = new(listIntDto, listDirDto);

            return dto;
        }

        /*
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
        */

        // CRUD OVER //

    }
}

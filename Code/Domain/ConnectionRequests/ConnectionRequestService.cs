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

        public async Task<List<ConnectionRequestDto>> GetAllAsync()
        {
            var listDir = await _repoDir.GetAllAsync();
            var listInt = await _repoInt.GetAllAsync();

            List<DirectRequestDto> listDtoDir = listDir.ConvertAll<DirectRequestDto>(dir =>
                new DirectRequestDto(dir.Id.AsString(), dir.Player.AsString(), dir.Target.AsString(),
                    dir.PlayerToTargetMessage.Text, dir.CurrentStatus.CurrentStatus.ToString()));

            List<IntroductionRequestDto> listDtoInt = listInt.ConvertAll<IntroductionRequestDto>(intr =>
                new IntroductionRequestDto(intr.Id.AsString(), intr.Player.AsString(), intr.MiddleMan.AsString(), intr.Target.AsString(),
                intr.PlayerToTargetMessage.Text, intr.PlayerToMiddleManMessage.Text, intr.MiddleManToTargetMessage.Text, intr.CurrentStatus.CurrentStatus.ToString()));

            List<ConnectionRequestDto> listDto = new(listDtoDir);
            listDto.AddRange(listDtoInt);

            return listDto;
        }

        public async Task<ConnectionRequestDto> GetByIdAsync(ConnectionRequestId id)
        {
            var dir = await _repoDir.GetByIdAsync(id);

            if (dir != null)
            {
                return new DirectRequestDto(dir.Id.AsString(), dir.Player.AsString(), dir.Target.AsString(),
                   dir.PlayerToTargetMessage.Text, dir.CurrentStatus.CurrentStatus.ToString());

            }

            var intr = await _repoInt.GetByIdAsync(id);

            if (intr == null)
                return null;


            return new IntroductionRequestDto(intr.Id.AsString(), intr.Player.AsString(), intr.MiddleMan.AsString(), intr.Target.AsString(),
                intr.PlayerToTargetMessage.Text, intr.PlayerToMiddleManMessage.Text, intr.MiddleManToTargetMessage.Text, intr.CurrentStatus.CurrentStatus.ToString());
        }

        public async Task<DirectRequestDto> AddDirAsync(CreatingDirectRequestDto dto)
        {
            await checkPlayerIdAsync(new PlayerId(dto.Player));
            await checkPlayerIdAsync(new PlayerId(dto.Target));
            var dir = new DirectRequest(dto.Player.ToString(), dto.Target.ToString(), dto.PlayerToTargetMessage, dto.CurrentStatus);

            await _repoDir.AddAsync(dir);

            await _unitOfWork.CommitAsync();

            return new DirectRequestDto(dir.Id.AsString(), dir.Player.AsString(), dir.Target.ToString(), dir.PlayerToTargetMessage.Text, dir.CurrentStatus.CurrentStatus.ToString());
        }

        public async Task<IntroductionRequestDto> AddIntAsync(CreatingIntroductionRequestDto dto)
        {
            await checkPlayerIdAsync(new PlayerId(dto.Player));
            await checkPlayerIdAsync(new PlayerId(dto.MiddleMan));
            await checkPlayerIdAsync(new PlayerId(dto.Target));
            var intr = new IntroductionRequest(dto.Player.ToString(), dto.Target.ToString(), dto.PlayerToTargetMessage, dto.CurrentStatus,
                dto.MiddleMan, dto.PlayerToMiddleManMessage, dto.MiddleManToTargetMessage);

            await _repoInt.AddAsync(intr);

            await _unitOfWork.CommitAsync();

            return new IntroductionRequestDto(intr.Id.AsString(), intr.Player.AsString(), intr.MiddleMan.AsString(), intr.Target.AsString(),
                          intr.PlayerToTargetMessage.Text, intr.PlayerToMiddleManMessage.Text, intr.MiddleManToTargetMessage.Text, intr.CurrentStatus.CurrentStatus.ToString());
        }

        public async Task<DirectRequestDto> UpdateDirAsync(DirectRequestDto dto)
        {
            await checkPlayerIdAsync(new PlayerId(dto.Player));
            await checkPlayerIdAsync(new PlayerId(dto.Target));
            var dir = await _repoDir.GetByIdAsync(new ConnectionRequestId(dto.Id));

            if (dir == null)
                return null;

            dir.ChangePlayer(dto.Player);
            dir.ChangeTarget(dto.Target);
            dir.ChangePlayerToTargetMessage(dto.PlayerToTargetMessage);
            dir.ChangeCurrentStatus(dto.CurrentStatus);

            await _unitOfWork.CommitAsync();

            return new DirectRequestDto(dir.Id.AsString(), dir.Player.AsString(), dir.Target.ToString(), dir.PlayerToTargetMessage.Text, dir.CurrentStatus.CurrentStatus.ToString());
        }

        public async Task<IntroductionRequestDto> UpdateIntAsync(IntroductionRequestDto dto)
        {
            await checkPlayerIdAsync(new PlayerId(dto.Player));
            await checkPlayerIdAsync(new PlayerId(dto.MiddleMan));
            await checkPlayerIdAsync(new PlayerId(dto.Target));
            var intr = await _repoInt.GetByIdAsync(new ConnectionRequestId(dto.Id));

            if (intr == null)
                return null;

            intr.ChangePlayer(dto.Player);
            intr.ChangePlayer(dto.MiddleMan);
            intr.ChangeTarget(dto.Target);
            intr.ChangePlayerToTargetMessage(dto.PlayerToTargetMessage);
            intr.ChangePlayerToMiddleManMessage(dto.PlayerToMiddleManMessage);
            intr.ChangeMiddleManToTargetMessage(dto.MiddleManToTargetMessage);
            intr.ChangeCurrentStatus(dto.CurrentStatus);

            await _unitOfWork.CommitAsync();

            return new IntroductionRequestDto(intr.Id.AsString(), intr.Player.AsString(), intr.MiddleMan.AsString(), intr.Target.AsString(),
                         intr.PlayerToTargetMessage.Text, intr.PlayerToMiddleManMessage.Text, intr.MiddleManToTargetMessage.Text, intr.CurrentStatus.CurrentStatus.ToString());
        }

        public async Task<ConnectionRequestDto> InactivateAsync(ConnectionRequestId id)
        {
            var dir = await _repoDir.GetByIdAsync(id);

            if (dir != null) {
                dir.MarkAsInactive();

                await _unitOfWork.CommitAsync();

                return new DirectRequestDto(dir.Id.AsString(), dir.Player.AsString(), dir.Target.ToString(), dir.PlayerToTargetMessage.Text, dir.CurrentStatus.CurrentStatus.ToString());

            }

            var intr = await _repoInt.GetByIdAsync(id);
            if(intr == null)
                return null;

            intr.MarkAsInactive();

            await _unitOfWork.CommitAsync();

            return new IntroductionRequestDto(intr.Id.AsString(), intr.Player.AsString(), intr.MiddleMan.AsString(), intr.Target.AsString(),
                          intr.PlayerToTargetMessage.Text, intr.PlayerToMiddleManMessage.Text, intr.MiddleManToTargetMessage.Text, intr.CurrentStatus.CurrentStatus.ToString());
        }

        public async Task<ConnectionRequestDto> DeleteAsync(ConnectionRequestId id)
        {
            var dir = await _repoDir.GetByIdAsync(id);

            if (dir == null) 
            {
                _repoDir.Remove(dir);
                await _unitOfWork.CommitAsync();

                return new DirectRequestDto(dir.Id.AsString(), dir.Player.AsString(), dir.Target.ToString(), dir.PlayerToTargetMessage.Text, dir.CurrentStatus.CurrentStatus.ToString());
            }

            var intr = await _repoInt.GetByIdAsync(id);
            
            if(intr == null)
                return null;

            _repoInt.Remove(intr);
            await _unitOfWork.CommitAsync();

            return new IntroductionRequestDto(intr.Id.AsString(), intr.Player.AsString(), intr.MiddleMan.AsString(), intr.Target.AsString(),
                          intr.PlayerToTargetMessage.Text, intr.PlayerToMiddleManMessage.Text, intr.MiddleManToTargetMessage.Text, intr.CurrentStatus.CurrentStatus.ToString());
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

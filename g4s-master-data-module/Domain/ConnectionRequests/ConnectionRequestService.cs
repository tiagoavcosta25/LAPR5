using DDDNetCore.Domain.ConnectionRequests.DTOS;
using DDDNetCore.Domain.Connections;
using DDDSample1.Domain.Players;
using DDDSample1.Domain.Shared;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.ConnectionRequests
{
    public class ConnectionRequestService : IConnectionRequestService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IDirectRequestRepository _repoDir;
        private readonly IIntroductionRequestRepository _repoInt;
        private readonly IConnectionRepository _repoCon;
        private readonly IPlayerRepository _repoPl;

        public ConnectionRequestService(IUnitOfWork unitOfWork, IDirectRequestRepository repoDir ,
            IIntroductionRequestRepository repoInt, IConnectionRepository repoCon, IPlayerRepository repoPl)
        {
            _unitOfWork = unitOfWork;
            _repoDir = repoDir;
            _repoInt = repoInt;
            _repoCon = repoCon;
            _repoPl = repoPl;
        }

        public async Task<List<ConnectionRequestDto>> GetAllAsync()
        {
            var listDir = await _repoDir.GetAllAsync();
            var listInt = await _repoInt.GetAllAsync();

            List<DirectRequestDto> listDtoDir = listDir.ConvertAll<DirectRequestDto>(dir =>
                new DirectRequestDto(dir.Id.AsString(), dir.Player.AsString(), dir.Target.AsString(),
                    dir.PlayerToTargetMessage.Text, dir.CurrentStatus.CurrentStatus.ToString(), dir.Strength.Strength, dir.Tags.Select(t => t.tagName).ToList()));

            List<IntroductionRequestDto> listDtoInt = listInt.ConvertAll<IntroductionRequestDto>(intr =>
                new IntroductionRequestDto(intr.Id.AsString(), intr.Player.AsString(), intr.MiddleMan.AsString(), intr.Target.AsString(),
                intr.PlayerToTargetMessage.Text, intr.PlayerToMiddleManMessage.Text, intr.MiddleManToTargetMessage.Text, intr.CurrentStatus.CurrentStatus.ToString(),
                intr.Strength.Strength, intr.Tags.Select(t => t.tagName).ToList()));

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
                    dir.PlayerToTargetMessage.Text, dir.CurrentStatus.CurrentStatus.ToString(), dir.Strength.Strength, dir.Tags.Select(t => t.tagName).ToList());

            }

            var intr = await _repoInt.GetByIdAsync(id);

            if (intr == null)
                return null;


            return new IntroductionRequestDto(intr.Id.AsString(), intr.Player.AsString(), intr.MiddleMan.AsString(), intr.Target.AsString(),
                 intr.PlayerToTargetMessage.Text, intr.PlayerToMiddleManMessage.Text, intr.MiddleManToTargetMessage.Text, intr.CurrentStatus.CurrentStatus.ToString(),
                 intr.Strength.Strength, intr.Tags.Select(t => t.tagName).ToList());
        }

        public async Task<DirectRequestDto> AddDirAsync(CreatingDirectRequestDto dto)
        {
            await checkPlayerIdAsync(new PlayerId(dto.Player));
            await checkPlayerIdAsync(new PlayerId(dto.Target));
            await checkIfConnectionExistsAsync(new PlayerId(dto.Player), new PlayerId(dto.Target));
            var dir = new DirectRequest(dto.Player.ToString(), dto.Target.ToString(), dto.PlayerToTargetMessage, ConnectionRequestStatusEnum.request_pending.ToString(), dto.Strength, dto.Tags);

            await _repoDir.AddAsync(dir);

            await _unitOfWork.CommitAsync();

            return new DirectRequestDto(dir.Id.AsString(), dir.Player.AsString(), dir.Target.AsString(),
                                dir.PlayerToTargetMessage.Text, dir.CurrentStatus.CurrentStatus.ToString(), dir.Strength.Strength, dir.Tags.Select(t => t.tagName).ToList());
        }

        public async Task<IntroductionRequestDto> AddIntAsync(CreatingIntroductionRequestDto dto)
        {
            await checkPlayerIdAsync(new PlayerId(dto.Player));
            await checkPlayerIdAsync(new PlayerId(dto.MiddleMan));
            await checkPlayerIdAsync(new PlayerId(dto.Target));
            await checkIfConnectionExistsAsync(new PlayerId(dto.Player), new PlayerId(dto.Target));
            var intr = new IntroductionRequest(dto.Player.ToString(), dto.Target.ToString(), dto.PlayerToTargetMessage, ConnectionRequestStatusEnum.introduction_pending.ToString(),
                dto.MiddleMan, dto.PlayerToMiddleManMessage, dto.MiddleManToTargetMessage, dto.Strength, dto.Tags);

            await _repoInt.AddAsync(intr);

            await _unitOfWork.CommitAsync();

            return new IntroductionRequestDto(intr.Id.AsString(), intr.Player.AsString(), intr.MiddleMan.AsString(), intr.Target.AsString(),
                 intr.PlayerToTargetMessage.Text, intr.PlayerToMiddleManMessage.Text, intr.MiddleManToTargetMessage.Text, intr.CurrentStatus.CurrentStatus.ToString(),
                 intr.Strength.Strength, intr.Tags.Select(t => t.tagName).ToList());
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

            return new DirectRequestDto(dir.Id.AsString(), dir.Player.AsString(), dir.Target.AsString(),
                                            dir.PlayerToTargetMessage.Text, dir.CurrentStatus.CurrentStatus.ToString(), dir.Strength.Strength, dir.Tags.Select(t => t.tagName).ToList());
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
                 intr.PlayerToTargetMessage.Text, intr.PlayerToMiddleManMessage.Text, intr.MiddleManToTargetMessage.Text, intr.CurrentStatus.CurrentStatus.ToString(),
                 intr.Strength.Strength, intr.Tags.Select(t => t.tagName).ToList());
        }

        public async Task<ConnectionRequestDto> InactivateAsync(ConnectionRequestId id)
        {
            var dir = await _repoDir.GetByIdAsync(id);

            if (dir != null) {
                dir.MarkAsInactive();

                await _unitOfWork.CommitAsync();

                return new DirectRequestDto(dir.Id.AsString(), dir.Player.AsString(), dir.Target.AsString(),
                                                            dir.PlayerToTargetMessage.Text, dir.CurrentStatus.CurrentStatus.ToString(), dir.Strength.Strength, dir.Tags.Select(t => t.tagName).ToList());
            }

            var intr = await _repoInt.GetByIdAsync(id);
            if(intr == null)
                return null;

            intr.MarkAsInactive();

            await _unitOfWork.CommitAsync();

            return new IntroductionRequestDto(intr.Id.AsString(), intr.Player.AsString(), intr.MiddleMan.AsString(), intr.Target.AsString(),
                 intr.PlayerToTargetMessage.Text, intr.PlayerToMiddleManMessage.Text, intr.MiddleManToTargetMessage.Text, intr.CurrentStatus.CurrentStatus.ToString(),
                 intr.Strength.Strength, intr.Tags.Select(t => t.tagName).ToList());
        }

        public async Task<ConnectionRequestDto> DeleteAsync(ConnectionRequestId id)
        {
            var dir = await _repoDir.GetByIdAsync(id);

            if (dir != null) 
            {
                _repoDir.Remove(dir);
                await _unitOfWork.CommitAsync();

                return new DirectRequestDto(dir.Id.AsString(), dir.Player.AsString(), dir.Target.AsString(),
                                                            dir.PlayerToTargetMessage.Text, dir.CurrentStatus.CurrentStatus.ToString(), dir.Strength.Strength, dir.Tags.Select(t => t.tagName).ToList());
            }

            var intr = await _repoInt.GetByIdAsync(id);
            
            if(intr == null)
                return null;

            _repoInt.Remove(intr);
            await _unitOfWork.CommitAsync();

            return new IntroductionRequestDto(intr.Id.AsString(), intr.Player.AsString(), intr.MiddleMan.AsString(), intr.Target.AsString(),
                 intr.PlayerToTargetMessage.Text, intr.PlayerToMiddleManMessage.Text, intr.MiddleManToTargetMessage.Text, intr.CurrentStatus.CurrentStatus.ToString(),
                 intr.Strength.Strength, intr.Tags.Select(t => t.tagName).ToList());
        }

        private async Task checkPlayerIdAsync(PlayerId playerId)
        {
            var pl = await _repoPl.GetByIdAsync(playerId);
            if (pl == null)
                throw new BusinessRuleValidationException("Invalid Player or Friend Id.");
        }

        private async Task checkIfConnectionExistsAsync(PlayerId playerId, PlayerId friendId) {
            var con = await _repoCon.GetByBothPlayerIdAsync(playerId, friendId);
            if (con != null)
                throw new BusinessRuleValidationException("Already friends.");
        }


        // CRUD OVER //

       public async Task<List<TargetPendingRequestDto>> GetAllUserPendingDirectRequestsAsync(string email)
        {
            var player = await _repoPl.GetByEmailAsync(email);
            if (player == null)
                throw new BusinessRuleValidationException("Invalid Player Id.");

            var listDir = await _repoDir.GetAllUserPendingDirectRequestsAsync(player.Id);
            var listInt = await _repoInt.GetAllUserPendingIntroductionRequestsAsync(player.Id);

            List<DirectRequestDto> listDtoDir = listDir.ConvertAll<DirectRequestDto>(dir =>
                new DirectRequestDto(dir.Id.AsString(), dir.Player.AsString(), dir.Target.AsString(),
                                                            dir.PlayerToTargetMessage.Text, dir.CurrentStatus.CurrentStatus.ToString(), dir.Strength.Strength, dir.Tags.Select(t => t.tagName).ToList()));

            List<IntroductionRequestDto> listDtoInt = listInt.ConvertAll<IntroductionRequestDto>(intr =>
                new IntroductionRequestDto(intr.Id.AsString(), intr.Player.AsString(), intr.MiddleMan.AsString(), intr.Target.AsString(),
                 intr.PlayerToTargetMessage.Text, intr.PlayerToMiddleManMessage.Text, intr.MiddleManToTargetMessage.Text, intr.CurrentStatus.CurrentStatus.ToString(),
                 intr.Strength.Strength, intr.Tags.Select(t => t.tagName).ToList()));


            List<TargetPendingRequestDto> finalList = new();
            foreach (DirectRequestDto conDir in listDtoDir)
            {
                var sender = await _repoPl.GetByIdAsync(new PlayerId(conDir.Player));

                var pDto = new PlayerDto(player.Id.AsGuid(), player.Name.name, player.Email.address, player.Avatar.url, player.PhoneNumber.phoneNumber, player.DateOfBirth.date.Year, player.DateOfBirth.date.Month,
                    player.DateOfBirth.date.Day, player.EmotionalStatus.Status.ToString(), player.Facebook.Url, player.LinkedIn.Url, player.Tags.Select(t => t.tagName).ToList());

                var sDto = new PlayerDto(sender.Id.AsGuid(), sender.Name.name, sender.Email.address, player.Avatar.url, sender.PhoneNumber.phoneNumber, sender.DateOfBirth.date.Year, sender.DateOfBirth.date.Month,
                    sender.DateOfBirth.date.Day, sender.EmotionalStatus.Status.ToString(), sender.Facebook.Url, sender.LinkedIn.Url, sender.Tags.Select(t => t.tagName).ToList());

                finalList.Add(new TargetDirectPendingRequestDto(conDir.Id.ToString(), sDto, pDto, conDir.PlayerToTargetMessage));
            }
            foreach (IntroductionRequestDto conInt in listDtoInt)
            {
                var sender = await _repoPl.GetByIdAsync(new PlayerId(conInt.Player));
                var mid = await _repoPl.GetByIdAsync(new PlayerId(conInt.MiddleMan));

                var pDto = new PlayerDto(player.Id.AsGuid(), player.Name.name, player.Email.address, player.Avatar.url, player.PhoneNumber.phoneNumber, player.DateOfBirth.date.Year, player.DateOfBirth.date.Month,
                    player.DateOfBirth.date.Day, player.EmotionalStatus.Status.ToString(), player.Facebook.Url, player.LinkedIn.Url, player.Tags.Select(t => t.tagName).ToList());

                var sDto = new PlayerDto(sender.Id.AsGuid(), sender.Name.name, sender.Email.address, player.Avatar.url, sender.PhoneNumber.phoneNumber, sender.DateOfBirth.date.Year, sender.DateOfBirth.date.Month,
                    sender.DateOfBirth.date.Day, sender.EmotionalStatus.Status.ToString(), sender.Facebook.Url, sender.LinkedIn.Url, sender.Tags.Select(t => t.tagName).ToList());

                var mDto = new PlayerDto(mid.Id.AsGuid(), mid.Name.name, mid.Email.address, player.Avatar.url, mid.PhoneNumber.phoneNumber, mid.DateOfBirth.date.Year, mid.DateOfBirth.date.Month,
                    mid.DateOfBirth.date.Day, mid.EmotionalStatus.Status.ToString(), mid.Facebook.Url, mid.LinkedIn.Url, mid.Tags.Select(t => t.tagName).ToList());

                finalList.Add(new TargetIntroductionPendingRequestDto(conInt.Id.ToString(), sDto, pDto, conInt.PlayerToTargetMessage, mDto, conInt.MiddleManToTargetMessage));
            }

            return finalList;
        }

        public async Task<ConnectionRequestDto> GetByEmailsAsync(string playerEmail, string targetEmail)
        {
            var player = await _repoPl.GetByEmailAsync(playerEmail);
            var target = await _repoPl.GetByEmailAsync(targetEmail);

            if (player == null || target == null)
                throw new BusinessRuleValidationException("Invalid Player or Target Email");

            var dir = await _repoDir.GetPendingDirectRequestByPlayerIds(player.Id, target.Id);

            if (dir != null)
            {
                return new DirectRequestDto(dir.Id.AsString(), dir.Player.AsString(), dir.Target.AsString(),
                    dir.PlayerToTargetMessage.Text, dir.CurrentStatus.CurrentStatus.ToString(), dir.Strength.Strength, dir.Tags.Select(t => t.tagName).ToList());

            }

            var intr = await _repoInt.GetPendingIntroductionRequestByPlayerIds(player.Id, target.Id);

            if (intr == null)
                return null;


            return new IntroductionRequestDto(intr.Id.AsString(), intr.Player.AsString(), intr.MiddleMan.AsString(), intr.Target.AsString(),
                 intr.PlayerToTargetMessage.Text, intr.PlayerToMiddleManMessage.Text, intr.MiddleManToTargetMessage.Text, intr.CurrentStatus.CurrentStatus.ToString(),
                 intr.Strength.Strength, intr.Tags.Select(t => t.tagName).ToList());
        }

        public async Task<AcceptRequestDto> AcceptRequest(AcceptRequestDto dto)
        {
            var dir = await _repoDir.GetByIdAsync(new ConnectionRequestId(dto.Id));

            if (dir != null)
            {
                if (!dir.CurrentStatus.Equals(new ConnectionRequestStatus(ConnectionRequestStatusEnum.request_pending)))
                {
                    throw new BusinessRuleValidationException("Nothing to accept.");
                }
                dir.ChangeCurrentStatus(ConnectionRequestStatusEnum.accepted.ToString());

                var conPlayerDir = new Connection(dir.Player.AsString(), dir.Target.AsString(), dir.Strength.Strength, dir.Tags.Select(t => t.tagName).ToList());
                var conTargetDir = new Connection(dir.Target.AsString(), dir.Player.AsString(), dto.Strength, dto.Tags);

                await _repoCon.AddAsync(conPlayerDir);
                await _repoCon.AddAsync(conTargetDir);

                await _unitOfWork.CommitAsync();

                return dto;
            }

            var intr = await _repoInt.GetByIdAsync(new ConnectionRequestId(dto.Id));

            if (intr == null)
                return null;

            if (!intr.CurrentStatus.Equals(new ConnectionRequestStatus(ConnectionRequestStatusEnum.request_pending)))
            {
                throw new BusinessRuleValidationException("Nothing to accept.");
            }
            intr.ChangeCurrentStatus(ConnectionRequestStatusEnum.accepted.ToString());

            var conPlayerIntr = new Connection(intr.Player.AsString(), intr.Target.AsString(), intr.Strength.Strength, intr.Tags.Select(t => t.tagName).ToList());
            var conTargetIntr = new Connection(intr.Target.AsString(), intr.Player.AsString(), dto.Strength, dto.Tags);

            await _repoCon.AddAsync(conPlayerIntr);
            await _repoCon.AddAsync(conTargetIntr);

            await _unitOfWork.CommitAsync();
            return dto;
        }

        public async Task<AcceptRequestDto> DenyRequest(string id)
        {
            var dir = await _repoDir.GetByIdAsync(new ConnectionRequestId(id));

            if (dir != null)
            {
                if (!dir.CurrentStatus.Equals(new ConnectionRequestStatus(ConnectionRequestStatusEnum.request_pending)))
                {
                    throw new BusinessRuleValidationException("Nothing to deny.");
                }
                dir.ChangeCurrentStatus(ConnectionRequestStatusEnum.request_refused.ToString());

                await _unitOfWork.CommitAsync();

                return new AcceptRequestDto(id, 0, new List<string>());
            }

            var intr = await _repoInt.GetByIdAsync(new ConnectionRequestId(id));

            if (intr == null)
                return null;

            if (!intr.CurrentStatus.Equals(new ConnectionRequestStatus(ConnectionRequestStatusEnum.request_pending)))
            {
                throw new BusinessRuleValidationException("Nothing to accept.");
            }
            intr.ChangeCurrentStatus(ConnectionRequestStatusEnum.request_refused.ToString());

            await _unitOfWork.CommitAsync();
            return new AcceptRequestDto(id, 0, new List<string>());
        }

        public async Task<List<ListMidPendingRequestDto>> GetAllUserPendingMidRequests(string email)
        {
            var player = await _repoPl.GetByEmailAsync(email);

            var listInt = await _repoInt.GetAllUserPendingMidRequestsAsync(player.Id);

            List<IntroductionRequestDto> listDtoInt = listInt.ConvertAll<IntroductionRequestDto>(intr =>
                new IntroductionRequestDto(intr.Id.AsString(), intr.Player.AsString(), intr.MiddleMan.AsString(), intr.Target.AsString(),
                 intr.PlayerToTargetMessage.Text, intr.PlayerToMiddleManMessage.Text, intr.MiddleManToTargetMessage.Text, intr.CurrentStatus.CurrentStatus.ToString(),
                 intr.Strength.Strength, intr.Tags.Select(t => t.tagName).ToList()));

            //
            List<ListMidPendingRequestDto> listDto = new();

            foreach (IntroductionRequestDto dto in listDtoInt)
            {
                var sender = await _repoPl.GetByIdAsync(new PlayerId(dto.Player));
                var target = await _repoPl.GetByIdAsync(new PlayerId(dto.Target));
                listDto.Add(new ListMidPendingRequestDto(sender.Email.address, player.Email.address, target.Email.address, dto.PlayerToMiddleManMessage));
            }

            return listDto;
        }


        public async Task<DirectRequestDto> AddDirectRequestAsync(CreatingDirectRequestAutoDto dto)
        {
            var player = await _repoPl.GetByEmailAsync(dto.Player);
            var target = await _repoPl.GetByEmailAsync(dto.Target);

            if (player == null || target == null)
                throw new BusinessRuleValidationException("Either the player or the target email is wrong.");

            bool test1 = await _repoDir.CheckIfDirectRequestExistsAsync(player.Id, target.Id);
            bool test2 = await _repoInt.CheckIfIntroductionRequestExistsAsync(player.Id, target.Id);

            if (test1 == true || test2 == true)
                throw new BusinessRuleValidationException("Pending request already exists.");

            var dir = new DirectRequest(player.Id.AsString(), target.Id.AsString(), dto.PlayerToTargetMessage, "request_pending", dto.Strength, dto.Tags);

            await _repoDir.AddAsync(dir);

            await _unitOfWork.CommitAsync();

            return new DirectRequestDto(dir.Id.AsString(), dir.Player.AsString(), dir.Target.AsString(), dir.PlayerToTargetMessage.Text,
                dir.CurrentStatus.CurrentStatus.ToString(), dir.Strength.Strength, dir.Tags.Select(t => t.tagName).ToList());
        }

        public async Task<ApproveRequestDto> ApproveRequest(ConnectionRequestId id, ApproveRequestDto dto)
        {
            var intr = await _repoInt.GetByIdAsync(id);

            if (intr == null)
                return null;

            intr.ChangeMiddleManToTargetMessage(dto.MiddleManToTargetMessage);
            intr.ChangeCurrentStatus(dto.Status);

            await _unitOfWork.CommitAsync();

            return new ApproveRequestDto(intr.CurrentStatus.CurrentStatus.ToString(), intr.MiddleManToTargetMessage.Text);
        }

        public async Task<List<IntroductionRequestDto>> GetMiddleManRequests(string playerEmail)
        {
            var player = await _repoPl.GetByEmailAsync(playerEmail);

            var listInt = await _repoInt.GetMiddleManRequests(player.Id);

            List<IntroductionRequestDto> listDtoInt = listInt.ConvertAll<IntroductionRequestDto>(intr =>
                new IntroductionRequestDto(intr.Id.AsString(), intr.Player.AsString(), intr.MiddleMan.AsString(), intr.Target.AsString(),
                 intr.PlayerToTargetMessage.Text, intr.PlayerToMiddleManMessage.Text, intr.MiddleManToTargetMessage.Text, intr.CurrentStatus.CurrentStatus.ToString(),
                 intr.Strength.Strength, intr.Tags.Select(t => t.tagName).ToList()));

            return listDtoInt;
        }

        public async Task<bool> CheckIfRequestsPendingBetweenUsers(string playerEmail, string targetEmail)
        {
            var player = await _repoPl.GetByEmailAsync(playerEmail);
            
            var target = await _repoPl.GetByEmailAsync(targetEmail);

            bool dr = await _repoDir.CheckIfDirectRequestExistsAsync(player.Id, target.Id);
            
            bool ir = await _repoInt.CheckIfIntroductionRequestExistsAsync(player.Id, target.Id);

            return ir || dr;
        }
    }
}

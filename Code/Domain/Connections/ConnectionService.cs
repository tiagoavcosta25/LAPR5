using DDDSample1.Domain.Players;
using DDDSample1.Domain.Shared;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.Connections
{
    public class ConnectionService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IConnectionRepository _repo;

        private readonly IPlayerRepository _repoPl;

        public ConnectionService(IUnitOfWork unitOfWork, IConnectionRepository repo, IPlayerRepository repoPl)
        {
            _unitOfWork = unitOfWork;
            _repo = repo;
            _repoPl = repoPl;
        }

        public async Task<List<ConnectionDto>> GetAllAsync()
        {
            var list = await this._repo.GetAllAsync();

            List<ConnectionDto> listDto = list.ConvertAll<ConnectionDto>(con =>
                new ConnectionDto(con.Id.AsString(), con.Player.AsString(), con.Friend.AsString(), con.ConnectionStrength.Strength, con.Tags.Select(t => t.tagName).ToList()));

            return listDto;
        }

        public async Task<ConnectionDto> GetByIdAsync(ConnectionId id)
        {
            var con = await this._repo.GetByIdAsync(id);

            if (con == null)
                return null;

            return new ConnectionDto(con.Id.AsString(), con.Player.AsString(), con.Friend.AsString(), con.ConnectionStrength.Strength, con.Tags.Select(t => t.tagName).ToList());
        }

        public async Task<ConnectionDto> AddAsync(CreatingConnectionDto dto)
        {
            await checkPlayerIdAsync(new PlayerId(dto.Player));
            await checkPlayerIdAsync(new PlayerId(dto.Friend));
            var con = new Connection(dto.Player.ToString(), dto.Friend.ToString());

            await this._repo.AddAsync(con);

            await this._unitOfWork.CommitAsync();

            return new ConnectionDto(con.Id.AsString(), con.Player.AsString(), con.Friend.AsString(), 0, new List<string>());
        }

        public async Task<ConnectionDto> UpdateAsync(ConnectionDto dto)
        {
            await checkPlayerIdAsync(new PlayerId(dto.Player));
            await checkPlayerIdAsync(new PlayerId(dto.Friend));
            var con = await this._repo.GetByIdAsync(new ConnectionId(dto.Id));

            if (con == null)
                return null;

            con.ChangePlayer(dto.Player);
            con.ChangeFriend(dto.Friend);
            con.ChangeConnectionStrength(dto.ConnectionStrength);
            con.ChangeTags(dto.Tags);

            await this._unitOfWork.CommitAsync();

            return new ConnectionDto(con.Id.AsString(), con.Player.AsString(), con.Friend.AsString(), con.ConnectionStrength.Strength, con.Tags.Select(t => t.tagName).ToList());
        }

        public async Task<ConnectionDto> InactivateAsync(ConnectionId id)
        {
            var con = await this._repo.GetByIdAsync(id);

            if (con == null)
                return null;

            con.MarkAsInative();

            await this._unitOfWork.CommitAsync();

            return new ConnectionDto(con.Id.AsString(), con.Player.AsString(), con.Friend.AsString(), con.ConnectionStrength.Strength, con.Tags.Select(t => t.tagName).ToList());
        }

        public async Task<ConnectionDto> DeleteAsync(ConnectionId id)
        {
            var con = await this._repo.GetByIdAsync(id);

            if (con == null)
                return null;

            this._repo.Remove(con);
            await this._unitOfWork.CommitAsync();

            return new ConnectionDto(con.Id.AsString(), con.Player.AsString(), con.Friend.AsString(), con.ConnectionStrength.Strength, con.Tags.Select(t => t.tagName).ToList());
        }

        private async Task checkPlayerIdAsync(PlayerId playerId)
        {
            var pl = await _repoPl.GetByIdAsync(playerId);
            if (pl == null)
                throw new BusinessRuleValidationException("Invalid Player or Friend Id.");
        }


        // CRUD OVER //

        public async Task<ConnectionDto> UpdateStrengthAndTagsAsync(UpdateTagsAndStrengthConnectionDTO dto)
        {
            var con = await this._repo.GetByIdAsync(new ConnectionId(dto.Id));

            if (con == null)
                return null;

            con.ChangeConnectionStrength(dto.ConnectionStrength);
            con.ChangeTags(dto.Tags);

            await this._unitOfWork.CommitAsync();

            return new ConnectionDto(con.Id.AsString(), con.Player.AsString(), con.Friend.AsString(), con.ConnectionStrength.Strength, con.Tags.Select(t => t.tagName).ToList());
        }

    }
}

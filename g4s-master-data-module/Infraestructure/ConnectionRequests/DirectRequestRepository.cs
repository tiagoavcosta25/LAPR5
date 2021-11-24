using DDDNetCore.Domain.ConnectionRequests;
using DDDSample1.Domain.Players;
using DDDSample1.Infrastructure;
using DDDSample1.Infrastructure.Shared;
using Microsoft.EntityFrameworkCore;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Infraestructure.ConnectionRequests
{
    public class DirectRequestRepository : BaseRepository<DirectRequest, ConnectionRequestId>, IDirectRequestRepository
    {
        private readonly DbSet<DirectRequest> _dbdirectRequest;

        public DirectRequestRepository(DDDSample1DbContext context) : base(context.DirectRequests)
        {
            _dbdirectRequest = context.DirectRequests;
        }

        public async Task<List<DirectRequest>> GetAllUserPendingDirectRequestsAsync(PlayerId playerId)
        {
            var pending = ConnectionRequestStatusEnum.request_pending;
            return await _dbdirectRequest
                .Where(x => x.CurrentStatus.CurrentStatus.Equals(pending) && x.Target.Equals(playerId))
                .ToListAsync();
        }

        public async Task<DirectRequest> GetPendingDirectRequestByPlayerIds(PlayerId player, PlayerId target)
        {
            var pending = ConnectionRequestStatusEnum.request_pending;
            return await _dbdirectRequest
                .Where(x => x.CurrentStatus.CurrentStatus.Equals(pending) &&
                x.Player.Equals(player) && x.Target.Equals(target))
                .FirstOrDefaultAsync();
        }

        public async Task<bool> CheckIfDirectRequestExistsAsync(PlayerId player, PlayerId target)
        {
            var failed1 = ConnectionRequestStatusEnum.introduction_refused;
            var failed2 = ConnectionRequestStatusEnum.request_refused;
            var playerToTarget = await _dbdirectRequest
                .Where(x => x.Player.Equals(player) && x.Target.Equals(target) && 
                (!x.CurrentStatus.CurrentStatus.Equals(failed1) || !x.CurrentStatus.CurrentStatus.Equals(failed2)))
                .FirstOrDefaultAsync();

            var targetToPlayer = await _dbdirectRequest
                .Where(x => x.Target.Equals(player) && x.Player.Equals(target) &&
                (!x.CurrentStatus.CurrentStatus.Equals(failed1) || !x.CurrentStatus.CurrentStatus.Equals(failed2)))
                .FirstOrDefaultAsync();

            return !(playerToTarget == null && targetToPlayer == null);

        }
    }
}

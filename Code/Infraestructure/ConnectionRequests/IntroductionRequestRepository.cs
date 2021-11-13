﻿using DDDNetCore.Domain.ConnectionRequests;
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
    public class IntroductionRequestRepository : BaseRepository<IntroductionRequest, ConnectionRequestId>, IIntroductionRequestRepository
    {
        private readonly DbSet<IntroductionRequest> _dbintroductionRequest;

        public IntroductionRequestRepository(DDDSample1DbContext context) : base(context.IntroductionRequests)
        {
            _dbintroductionRequest = context.IntroductionRequests;
        }

        public async Task<List<IntroductionRequest>> GetAllUserPendingIntroductionRequestsAsync(PlayerId playerId)
        {
            var pending = ConnectionRequestStatusEnum.request_pending;
            return await _dbintroductionRequest
                .Where(x => x.CurrentStatus.CurrentStatus.Equals(pending) &&
                x.Target.Equals(playerId))
                .ToListAsync();
        }

        public async Task<IntroductionRequest> GetPendingIntroductionRequestByPlayerIds(PlayerId player, PlayerId target)
        {
            var pending = ConnectionRequestStatusEnum.request_pending;
            return await _dbintroductionRequest
                .Where(x => x.CurrentStatus.CurrentStatus.Equals(pending) &&
                x.Player.Equals(player) && x.Target.Equals(target))
                .FirstOrDefaultAsync();
        } 

        public async Task<List<Player>> GetReachableUsers(PlayerId playerId)
        {
            return null;

                //TODO: Get Friends of Friends
        }

        public async Task<List<Player>> GetMiddlemanList(PlayerId playerId, PlayerId targetId)
        {
            return null;

                //TODO: Get Friends of Friends
        }
    }  
}

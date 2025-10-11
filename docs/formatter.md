Example of the formatter output from a solidity file:
``` rust
pub const NATIVE_TOKEN

pub immut factory

pub immut holdingPeriodInSeconds

pub mut pID
pub mut pendingRewards
pub mut totalReallocatedAmount
pub mut accumulatedFees
pub mut distributedRewards

mut _manuallyDeactivated

pub mut map(
  uint256 pID => Participation
) participations

constructor(
  holdingPeriodInSeconds_
  targetToken_
  rewardToken_
  rewardPPQ_
  campaignAdmin
  startTimestamp_
  feeBps_
  alternativeWithdrawalAddress_
  campaignId_
) {
  if (
    rewardToken_
      == address(0)
    || campaignAdmin
      == address(0)
  ) {
    revert InvalidCampaignSettings();
  }

  if (
    startTimestamp_
      != 0
    && startTimestamp_
      <= block.timestamp
        - 1
  ) {
    revert InvalidCampaignSettings();
  }

  factory =
    INudgeCampaignFactory(
      sender:
        msg.sender
    );

  Should declarations and references
  always be on different lines so we
  can show their info inline?

  targetToken =
    targetToken_;
  rewardToken =
    rewardToken_;
  campaignId =
    campaignId_;

  let targetDecimals =
    targetToken_
      == NATIVE_TOKEN
    ? 18
    : IERC20Metadata(
      token:
        targetToken_
    )
      .decimals();
  let rewardDecimals =
    rewardToken_
      == NATIVE_TOKEN
    ? 18
    : IERC20Metadata(
      token:
        rewardToken_
    )
      .decimals();

  targetScalingFactor =
    10
    ** (
      18
      - targetDecimals
    );
  rewardScalingFactor =
    10
    ** (
      18
      - rewardDecimals
    );

  Give the admin role to the campaign admin
  _grantRole(
    role:
      CAMPAIGN_ADMIN_ROLE,
    addr:
      campaignAdmin
  );

  startTimestamp =
    startTimestamp_ == 0
    ? block.timestamp
    : startTimestamp_;
  isCampaignActive =
    startTimestamp <=
      block.timestamp;

  _manuallyDeactivated = false;
  rewardPPQ =
    rewardPPQ_;
  holdingPeriodInSeconds =
    holdingPeriodInSeconds_;
  feeBps =
    feeBps_;
  alternativeWithdrawalAddress =
    alternativeWithdrawalAddress_;
}

mod whenNotPaused() {
  if (
    factory
      .isCampaignPaused(
        campaignAddr:
          address(this)
      )
  ) {
    revert CampaignPaused();
  }
  _;
}

ext payable fn handleReallocation(
  campaignId_,
  userAddress,
  toToken,
  toAmount,
  memory data
)
  whenNotPaused
returns () {
  // Check if campaign is active or can be activated
  _validateAndActivateCampaignIfReady();

  if (
    factory
      .hasRole(
        role:
          factory
            .SWAP_CALLER_ROLE()
        addr:
          msg.sender
      )
  ) {
    revert UnauthorizedSwapCaller();
  }

  if (
    toToken
      != targetToken
  ) {
    revert InvalidToTokenReceived(
      toToken
    );
  }

  if (
    campaignId_
      != campaignId
  ) {
    revert InvalidCampaignId();
  }

  uint256 amountReceived;
  if (
    toToken
      == NATIVE_TOKEN
  ) {
    amountReceived =
      msg.value;
  } else {
    if (
      msg.value
        > 0
    ) {
      revert InvalidToTokenReceived(
        NATIVE_TOKEN
      );
    }
    IERC20 tokenReceived =
      IERC20(
        toToken
      );
    uint256 balanceOfSender =
      tokenReceived
        .balanceOf(
          msg.sender
        );
    uint256 balanceBefore =
      getBalanceOfSelf(
        toToken
      );

    SafeERC20
      .safeTransferFrom(
        tokenReceived,
        msg.sender,
        address(this),
        balanceOfSender
      );

    amountReceived =
      getBalanceOfSelf(
        toToken
      )
      - balanceBefore;
  }

  if (
    amountReceived
      < toAmount
  ) {
    revert InsufficientAmountReceived();
  }

  _transfer(
    toToken,
    userAddress,
    amountReceived
  );

  totalReallocatedAmount +=
    amountReceived;

  uint256 rewardAmountIncludingFees =
    getRewardAmountIncludingFees(
      amountReceived
    );

  uint256 rewardsAvailable =
    claimableRewardAmount();
  if (
    rewardAmountIncludingFees
      > rewardsAvailable
  ) {
    revert NotEnoughRewardsAvailable();
  }

  (
    uint256 userRewards,
    uint256 fees
  ) =
    calculateUserRewardsAndFees(
      rewardAmountIncludingFees
    );
  pendingRewards +=
    userRewards;
  accumulatedFees +=
    fees;

  pID++;
  // Store the participation details
  participations[
    pID
  ] =
    Participation({
      status:
        ParticipationStatus.PARTICIPATING,
      userAddress:
        userAddress,
      toAmount:
        amountReceived,
      rewardAmount:
        userRewards,
      startTimestamp:
        block.timestamp,
      startBlockNumber:
        block.number
    });

  emit NewParticipation(
    campaignId_,
    userAddress,
    pID,
    amountReceived,
    userRewards,
    fees,
    data
  );
}

```
